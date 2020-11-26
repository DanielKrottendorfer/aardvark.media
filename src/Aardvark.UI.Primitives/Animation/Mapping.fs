﻿namespace Aardvark.UI.Anewmation

open Aardvark.Base
open FSharp.Data.Adaptive
open System
open Aether

type private Mapping<'Model, 'T, 'U> =
    {
        /// The current state of the animation.
        CurrentState : State

        /// Input animation.
        Input : IAnimation<'Model, 'T>

        /// Mapping from input to output.
        Mapping : Func<'T, 'U>

        /// Observers to be notified of changes.
        Observers : HashMap<IAnimationObserver<'Model>, IAnimationObserver<'Model, 'U>>
    }

    /// Stops the animation and resets it.
    member x.Stop() =
        { x with Input = x.Input.Stop() }

    /// Starts the animation from the beginning (i.e. sets its start time to the given global time).
    member x.Start(globalTime) =
        { x with Input = x.Input.Start(globalTime) }

    /// Pauses the animation if it is running or has started.
    member x.Pause(globalTime) =
        { x with Input = x.Input.Pause(globalTime) }

    /// Resumes the animation from the point it was paused.
    /// Has no effect if the animation is not paused.
    member x.Resume(globalTime) =
        { x with Input = x.Input.Resume(globalTime) }

    /// Updates the animation to the given global time.
    member x.Update(globalTime) =
        { x with Input = x.Input.Update(globalTime) }

    /// Updates the distance-time function of the animation according to the given mapping.
    member x.DistanceTime(mapping : IDistanceTimeFunction -> IDistanceTimeFunction) =
        { x with Input = x.Input.DistanceTime(mapping) }

    /// Registers a new observer.
    member x.Subscribe(observer : IAnimationObserver<'Model, 'U>) =
        if not observer.IsEmpty then
            let key = observer :> IAnimationObserver<'Model>
            { x with Observers = x.Observers |> HashMap.add key observer }
        else
            x

    /// Removes all observers.
    member x.UnsubscribeAll() =
        { x with
            Input = x.Input.UnsubscribeAll()
            Observers = HashMap.empty }

    /// Removes the given observer (if present).
    member x.Unsubscribe(observer : IAnimationObserver<'Model>) =
        { x with
            Input = x.Input.Unsubscribe(observer)
            Observers = x.Observers |> HashMap.remove observer }

    /// Notifies all observers.
    member x.Notify(lens : Lens<'Model, IAnimation<'Model>>, name : Symbol, model : 'Model) =

        let input, model =
            let model = x.Input.Notify(lens, name, model)
            let input = model |> Optic.get lens |> unbox<IAnimation<'Model, 'T>>

            input, model |> Optic.set lens ({ x with CurrentState = input.State; Input = input } :> IAnimation<'Model>)

        let events =
            (x.CurrentState, input.State) ||> Events.compute |> List.sort

        let notify value model event =
            (model, x.Observers |> HashMap.toSeq)
            ||> Seq.fold (fun model (_, obs) -> obs.OnNext(model, name, event, value))

        if List.isEmpty events then
            model
        else
            let value = x.Mapping.Invoke(input.Value)
            (model, events) ||> Seq.fold (notify value)

    interface IAnimation<'Model> with
        member x.State = x.CurrentState
        member x.Stop() = x.Stop() :> IAnimation<'Model>
        member x.Start(globalTime) = x.Start(globalTime) :> IAnimation<'Model>
        member x.Pause(globalTime) = x.Pause(globalTime) :> IAnimation<'Model>
        member x.Resume(globalTime) = x.Resume(globalTime) :> IAnimation<'Model>
        member x.Update(globalTime) = x.Update(globalTime) :> IAnimation<'Model>
        member x.DistanceTime(mapping) = x.DistanceTime(mapping) :> IAnimation<'Model>
        member x.Notify(lens, name, model) = x.Notify(lens, name, model)
        member x.Unsubscribe(observer) = x.Unsubscribe(observer) :> IAnimation<'Model>
        member x.UnsubscribeAll() = x.UnsubscribeAll() :> IAnimation<'Model>

    interface IAnimation<'Model, 'U> with
        member x.Value = x.Input.Value |> x.Mapping.Invoke
        member x.Stop() = x.Stop() :> IAnimation<'Model, 'U>
        member x.Start(globalTime) = x.Start(globalTime) :> IAnimation<'Model, 'U>
        member x.Pause(globalTime) = x.Pause(globalTime) :> IAnimation<'Model, 'U>
        member x.Resume(globalTime) = x.Resume(globalTime) :> IAnimation<'Model, 'U>
        member x.Update(globalTime) = x.Update(globalTime) :> IAnimation<'Model, 'U>
        member x.DistanceTime(mapping) = x.DistanceTime(mapping) :> IAnimation<'Model, 'U>
        member x.Subscribe(observer) = x.Subscribe(observer) :> IAnimation<'Model, 'U>
        member x.Unsubscribe(observer) = x.Unsubscribe(observer) :> IAnimation<'Model, 'U>
        member x.UnsubscribeAll() = x.UnsubscribeAll() :> IAnimation<'Model, 'U>

type private Mapping2<'Model, 'T1, 'T2, 'U> =
    {
        /// The current state of the inputs.
        States : struct (State * State)

        /// Input animations.
        Inputs : struct (IAnimation<'Model, 'T1> * IAnimation<'Model, 'T2>)

        /// Mapping from input to output.
        Mapping : Func<'T1, 'T2, 'U>

        /// Observers to be notified of changes.
        Observers : HashMap<IAnimationObserver<'Model>, IAnimationObserver<'Model, 'U>>
    }

    member private x.StateA = let struct (a, _) = x.States in a
    member private x.StateB = let struct (_, b) = x.States in b
    member private x.InputA = let struct (a, _) = x.Inputs in a
    member private x.InputB = let struct (_, b) = x.Inputs in b

    /// Returns the state of the animation.
    member x.State =
        GroupState.get [x.StateA; x.StateB]

    /// Stops the animation and resets it.
    member x.Stop() =
        { x with Inputs = struct (x.InputA.Stop(), x.InputB.Stop()) }

    /// Starts the animation from the beginning (i.e. sets its start time to the given global time).
    member x.Start(globalTime) =
        { x with Inputs = struct (x.InputA.Start(globalTime), x.InputB.Start(globalTime)) }

    /// Pauses the animation if it is running or has started.
    member x.Pause(globalTime) =
        { x with Inputs = struct (x.InputA.Pause(globalTime), x.InputB.Pause(globalTime)) }

    /// Resumes the animation from the point it was paused.
    /// Has no effect if the animation is not paused.
    member x.Resume(globalTime) =
        { x with Inputs = struct (x.InputA.Resume(globalTime), x.InputB.Resume(globalTime)) }

    /// Updates the animation to the given global time.
    member x.Update(globalTime) =
        { x with Inputs = struct (x.InputA.Update(globalTime), x.InputB.Update(globalTime)) }

    /// Updates the distance-time function of the animation according to the given mapping.
    member x.DistanceTime(mapping : IDistanceTimeFunction -> IDistanceTimeFunction) =
        { x with Inputs = struct (x.InputA.DistanceTime(mapping), x.InputB.DistanceTime(mapping)) }

    /// Registers a new observer.
    member x.Subscribe(observer : IAnimationObserver<'Model, 'U>) =
        if not observer.IsEmpty then
            let key = observer :> IAnimationObserver<'Model>
            { x with Observers = x.Observers |> HashMap.add key observer }
        else
            x

    /// Removes all observers.
    member x.UnsubscribeAll() =
        { x with
            Inputs = struct(x.InputA.UnsubscribeAll(), x.InputB.UnsubscribeAll())
            Observers = HashMap.empty }

    /// Removes the given observer (if present).
    member x.Unsubscribe(observer : IAnimationObserver<'Model>) =
        { x with
            Inputs = struct (x.InputA.Unsubscribe(observer), x.InputB.Unsubscribe(observer))
            Observers = x.Observers |> HashMap.remove observer }

    /// Notifies all observers.
    member x.Notify(lens : Lens<'Model, IAnimation<'Model>>, name : Symbol, model : 'Model) =

        let inputA, inputB, model =
            let model = x.InputA.Notify(lens, name, model)
            let inputA = model |> Optic.get lens |> unbox<IAnimation<'Model, 'T1>>

            let model = x.InputB.Notify(lens, name, model)
            let inputB = model |> Optic.get lens |> unbox<IAnimation<'Model, 'T2>>

            inputA, inputB, model

        let next = { x with Inputs = struct (inputA, inputB); States = struct (inputA.State, inputB.State) }
        let model = model |> Optic.set lens (next :> IAnimation<'Model>)

        let events =
            let perMember = [
                    (x.StateA, next.StateA) ||> Events.compute
                    (x.StateB, next.StateB) ||> Events.compute
                ]
            let perGroup = [next.StateA; next.StateB] |> GroupState.get |> Events.compute x.State

            perGroup
            |> List.filter (function
                | EventType.Start -> perMember |> List.forall (List.contains EventType.Start)   // Group only starts if ALL members start
                | _ -> true
            )
            |> List.sort

        let notify value model event =
            (model, x.Observers |> HashMap.toSeq)
            ||> Seq.fold (fun model (_, obs) -> obs.OnNext(model, name, event, value))

        if List.isEmpty events then
            model
        else
            let value = x.Mapping.Invoke(inputA.Value, inputB.Value)
            (model, events) ||> Seq.fold (notify value)

    interface IAnimation<'Model> with
        member x.State = x.State
        member x.Stop() = x.Stop() :> IAnimation<'Model>
        member x.Start(globalTime) = x.Start(globalTime) :> IAnimation<'Model>
        member x.Pause(globalTime) = x.Pause(globalTime) :> IAnimation<'Model>
        member x.Resume(globalTime) = x.Resume(globalTime) :> IAnimation<'Model>
        member x.Update(globalTime) = x.Update(globalTime) :> IAnimation<'Model>
        member x.DistanceTime(mapping) = x.DistanceTime(mapping) :> IAnimation<'Model>
        member x.Notify(lens, name, model) = x.Notify(lens, name, model)
        member x.Unsubscribe(observer) = x.Unsubscribe(observer) :> IAnimation<'Model>
        member x.UnsubscribeAll() = x.UnsubscribeAll() :> IAnimation<'Model>

    interface IAnimation<'Model, 'U> with
        member x.Value = (x.InputA.Value, x.InputB.Value) |> x.Mapping.Invoke
        member x.Stop() = x.Stop() :> IAnimation<'Model, 'U>
        member x.Start(globalTime) = x.Start(globalTime) :> IAnimation<'Model, 'U>
        member x.Pause(globalTime) = x.Pause(globalTime) :> IAnimation<'Model, 'U>
        member x.Resume(globalTime) = x.Resume(globalTime) :> IAnimation<'Model, 'U>
        member x.Update(globalTime) = x.Update(globalTime) :> IAnimation<'Model, 'U>
        member x.DistanceTime(mapping) = x.DistanceTime(mapping) :> IAnimation<'Model, 'U>
        member x.Subscribe(observer) = x.Subscribe(observer) :> IAnimation<'Model, 'U>
        member x.Unsubscribe(observer) = x.Unsubscribe(observer) :> IAnimation<'Model, 'U>
        member x.UnsubscribeAll() = x.UnsubscribeAll() :> IAnimation<'Model, 'U>


[<AutoOpen>]
module AnimationMappingExtensions =

    module Animation =

        /// Returns a new animation that applies the mapping function to the input animation.
        let map (mapping : 'T -> 'U) (animation : IAnimation<'Model, 'T>) =
            { CurrentState = animation.State
              Input = animation
              Mapping = Func<_,_> mapping
              Observers = HashMap.empty } :> IAnimation<'Model, 'U>

        /// Returns a new animation that applies the mapping function to the input animations.
        let map2 (mapping : 'T1 -> 'T2 -> 'U) (x : IAnimation<'Model, 'T1>) (y : IAnimation<'Model, 'T2>) =
            { States = struct (x.State, y.State)
              Inputs = struct (x, y)
              Mapping = Func<_,_,_> mapping
              Observers = HashMap.empty } :> IAnimation<'Model, 'U>

        /// Returns a new animation that applies the mapping function to the input animations.
        let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'U) (x : IAnimation<'Model, 'T1>) (y : IAnimation<'Model, 'T2>) (z : IAnimation<'Model, 'T3>) =
            z |> map2 (fun (a, b) c -> mapping a b c) (
                map2 (fun a b -> (a, b)) x y
            )