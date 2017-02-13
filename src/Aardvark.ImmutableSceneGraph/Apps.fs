﻿namespace Scratch

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application

open Scratch.DomainTypes

open Aardvark.ImmutableSceneGraph
open Aardvark.Elmish
open Primitives

module SimpleDrawingApp =

    open Aardvark.ImmutableSceneGraph
    open Aardvark.Elmish
    open Primitives

    open SimpleDrawingApp

    type Action =
        | ClosePolygon
        | AddPoint   of V3d
        | MoveCursor of V3d

    let update (picking : Option<int>) e (m : Model) (cmd : Action) =
        match cmd, picking with
            | ClosePolygon, _ -> 
                match m.working with
                    | None -> m
                    | Some p -> 
                        { m with 
                            working = None 
                            finished = PSet.add p.finishedPoints m.finished
                        }
            | AddPoint p, Some _ ->
                match m.working with
                    | None -> { m with working = Some { finishedPoints = [ p ]; cursor = None;  }}
                    | Some v -> 
                        { m with working = Some { v with finishedPoints = p :: v.finishedPoints }}
            | MoveCursor p, Some _ ->
                match m.working with
                    | None -> { m with working = Some { finishedPoints = []; cursor = Some p }}
                    | Some v -> { m with working = Some { v with cursor = Some p }}
            | _,_ -> m


    let viewPolygon (p : list<V3d>) =
        [ for edge in Polygon3d(p |> List.toSeq).EdgeLines do
            let v = edge.P1 - edge.P0
            yield Primitives.cylinder edge.P0 v.Normalized v.Length 0.03 |> Scene.render Pick.ignore 
        ] |> Scene.group

    let viewDrawingPolygons (m : MModel) =
        aset {
            for p in m.mfinished :> aset<_> do yield viewPolygon p

            let! working = m.mworking
            match working with
                | Some v when v.cursor.IsSome -> 
                    yield 
                        [ Sphere3d(V3d.OOO,0.1) |> Sphere |>  Scene.render Pick.ignore ] 
                            |> Scene.colored (Mod.constant C4b.Red)
                            |> Scene.transform' (Mod.constant <| Trafo3d.Translation(v.cursor.Value))
                    yield viewPolygon (v.cursor.Value :: v.finishedPoints)
                | _ -> ()
        }
        
    let viewPlane = [ Quad (Quad3d [| V3d(-2,-2,0); V3d(2,-2,0); V3d(2,2,0); V3d(-2,2,0) |]) 
                            |>  Scene.render [ 
                                 on Mouse.move MoveCursor
                                 on (Mouse.down' MouseButtons.Left)  AddPoint 
                               //  on (Mouse.down' MouseButtons.Right) (constF ClosePolygon)
                               ] 
                      ] |>  Scene.colored (Mod.constant C4b.Gray)

    let view (m : MModel) = 
        let t = viewDrawingPolygons m
        Scene.agroup  t

    let viewScene (sizes : IMod<V2i>) (m : MModel) =
        let cameraView = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI |> Mod.constant
        let frustum = sizes |> Mod.map (fun (b : V2i) -> Frustum.perspective 60.0 0.1 10.0 (float b.X / float b.Y))
        view m
            |> Scene.camera (Mod.map2 Camera.create cameraView frustum)
            |> Scene.effect [toEffect DefaultSurfaces.trafo; toEffect DefaultSurfaces.vertexColor; toEffect DefaultSurfaces.simpleLighting]


    let subscriptions (m : Model) =
        Many [Input.key Down Keys.Enter (fun _ _-> ClosePolygon)]

    let initial = { finished = PSet.empty; working = None; _id = null }

    let app s =
        {
            initial = initial
            update = update (Some 0)
            view = viewScene s
            ofPickMsg = fun _ _ -> []
            subscriptions = subscriptions
        }

module TestApp =

    open Fablish
    open Fable.Helpers.Virtualdom
    open Fable.Helpers.Virtualdom.Html

    open SharedModel

    type Model = SharedModel.Ui

    type Action = Inc | Dec | Reset | SetInfo of string

    let update e (m : Model) (a : Action) =
        match a with
            | Inc -> { m with cnt = m.cnt + 1 }
            | Dec -> { m with cnt = m.cnt - 1 }
            | Reset -> { m with cnt = 0 }
            | SetInfo info -> { m with info = info}

    let view (m : Model) : DomNode<Action> =
        div [] [
            div [Style ["width", "100%"; "height", "100%"; "background-color", "transparent"]; attribute "id" "renderControl"] [
                text (sprintf "current content: %d" m.cnt)
                br []
                button [onMouseClick (fun dontCare -> Inc); attribute "class" "ui button"] [text "increment"]
                button [onMouseClick (fun dontCare -> Dec)] [text "decrement"]
                button [onMouseClick (fun dontCare -> Reset)] [text "reset"]
                br []
                text (sprintf "ray: %s" m.info)
            ]
        ]

    let initial = { info = "not known"; cnt = 0; _id = null }

    let app =
        {
            initial = initial
            update = update 
            view = view
            subscriptions = Subscriptions.none
            onRendered = OnRendered.ignore
        }

module PlaceTransformObjects =

    open TranslateController
    open PlaceTransformObjects
    open Aardvark.ImmutableSceneGraph.Scene

    (*
    issues:
    * domaintypes not working for tuples, not for namespaces
    * render neeeds adaptive variant
    * pset find
    * pset updateAt
    * mod IDs in actions?
    *)
    
    module PSet =
        let findByUpdate f g pset =
            pset 
                |> PSet.toList 
                |> List.map (fun v -> 
                        if f v then g v else v
                    )
                |> PSet.ofList 

        let findBy f pset =
            pset |> PSet.toList |> List.find f

        let pick f pset =
            pset |> PSet.toList |> List.pick f 

    let initial =
        {
            objects = PSet.ofList [ { id = 0; t = Trafo3d.Translation V3d.OOO; _id = null } ]
            hoveredObj = None
            selectedObj = None
            _id = null
        }

    type Action =
        | PlaceObject of V3d
        | SelectObject of int
        | HoverObject  of int
        | Unselect
        | Unhover
        | TransformObject of int * TranslateController.Action

    let update e (m : Model) (msg : Action) =
        match msg with
            | PlaceObject p -> { m with objects = PSet.add { id = PSet.count m.objects; t = Trafo3d.Translation p; _id = null }  m.objects }
            | SelectObject i -> 
                { m with selectedObj = Some { _id = null; id = i; tmodel = { TranslateController.initalModel with trafo = PSet.pick (fun a -> if a.id = i then Some a.t else None) m.objects }} }
            | TransformObject(index,translation) ->
                match m.selectedObj with
                    | Some old ->
                        let t = TranslateController.updateModel e old.tmodel translation
                        { m with 
                            selectedObj = Some { old with tmodel = t }
                            objects = PSet.findByUpdate (fun a -> a.id = old.id) (fun a -> { a with t = t.trafo }) m.objects }
                    | _ -> m
            | HoverObject i -> { m with hoveredObj = Some i }
            | Unhover -> { m with hoveredObj = None }
            | Unselect -> 
                { m with selectedObj = None }

    let isSelected (m : Option<int>) i =
        match m with
            | Some m when m = i -> true
            | _ -> false

    let isHovered m i =
        match m with
            | Some s when s = i -> true
            | _ -> false

    let viewObjects (m : MModel) : aset<ISg<Action>> =
        aset {
            for o in m.mobjects :> aset<_> do
                let! i = o.mid
                let! selected = m.mselectedObj
                let id = 
                    m.mselectedObj |> Mod.bind (fun a -> 
                        match a with 
                            | None -> Mod.constant None
                            | Some v -> (v.mid :> IMod<_> |> Mod.map Some)
                    )
                let color = Mod.map2 (fun s h -> if isSelected s i then C4b.Red elif isHovered h i then C4b.Blue else C4b.Gray) id m.mhoveredObj
                yield Sphere3d(V3d.OOO,0.1) 
                       |> Sphere 
                       |> render [
                            yield on (Mouse.down' MouseButtons.Right) (constF Unselect)
                            if selected.IsNone then 
                                yield on Mouse.move (fun _ -> HoverObject i)
                                yield on (Mouse.down' MouseButtons.Left) (fun _ -> SelectObject i)
                           ]
                       |> transform' o.mt
                       |> colored' color
        }


    let view (m : MModel) =
        aset {
            yield! viewObjects m
            yield 
                Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) 
                 |> render [ 
                        on (Mouse.down' MouseButtons.Middle) PlaceObject 
                    ] 
                 |> colored' (Mod.constant C4b.Gray)
            let! selected = m.mselectedObj
            match selected with
                | None -> ()
                | Some s -> 
                    yield TranslateController.viewModel s.mtmodel |> Scene.map (fun a -> TransformObject(s.mid.Value,a))
        } |> agroup

    let ofPickMsgModel (m : Model) (pick : GlobalPick) =
        [
            match m.selectedObj with
                | None -> 
                    yield Unhover
                | Some o -> 
                    match pick.mouseEvent with
                        | MouseEvent.Click MouseButtons.Right -> 
                            yield Unselect
                        | _ ->
                            yield! TranslateController.ofPickMsgModel o.tmodel pick |> List.map (fun a -> TransformObject(o.id,a))
        ]

    let app =
        {
            initial = initial
            update = update

            view = view
            ofPickMsg = ofPickMsgModel 
            subscriptions = Subscriptions.none
        }

module OrbitCameraApp = 

    open Aardvark.Base
    open Aardvark.Base.Rendering

    open Scratch.DomainTypes2
    open CameraTest
    open Primitives
    open Aardvark.ImmutableSceneGraph.Scene

    open Input

    type Action =                 
        | MouseDelta of V2d
        | PickPoint  of V3d
        | Animate    of DateTime
        | TimeStep   of float    

    let center' (c) = 
        match c with
            | Some s -> s
            | None -> V3d.Zero       
            
    let view (m : MModel) =        
        [Sphere (Sphere3d(V3d.OOO, 1.0)) 
            |> Scene.render [ on (Mouse.down' MouseButtons.Left) PickPoint ]
         Sphere (Sphere3d(V3d.OOO, 0.02)) 
            |> Scene.render [] 
            |> colored' (Mod.constant C4b.Red)
            |> Scene.transform' (m.mcenter |> Mod.map (fun a -> Trafo3d.Translation (center' a)))
        ]
        |> Scene.group            
        //|> Scene.viewTrafo (m.mcamera |> Mod.map CameraView.viewTrafo)
        //|> Scene.projTrafo (m.mfrustum |> Mod.map Frustum.projTrafo)

    let viewCenter (m : MModel) =
         Sphere (Sphere3d(V3d.OOO, 0.02)) 
                |> Scene.render [] 
                |> colored' (Mod.constant C4b.Red)
                |> Scene.transform' (m.mcenter |> Mod.map (fun a -> Trafo3d.Translation (center' a)))
    
    let orientationFctr = 1.0
    let panningFctr = 1.0
    let zoomingFctr = 8.0

    let clampedLocation (p:V3d) (s:V3d) (c:V3d) =
        let newLoc = p + s;
        let dist = (c - newLoc).Length
        let stepDist = (newLoc-p).Length
        
        if stepDist < dist then newLoc else p

    let (|Orient|Panning|Zoom|NoOp|) (look, pan, zoom, center) =
        match look,pan,zoom,center with
            | Some _, None,   None,   Some c -> Orient  c
            | None,   Some _, None,   Some c -> Panning c
            | None,   None,   Some _, Some c -> Zoom    c
            | _,_,_,_                        -> NoOp

    let update pickEx e (m : Model) msg = 
        match msg with            
            | MouseDelta d -> 
                match (m.lookingAround, m.panning, m.zooming, m.center) with
                    | Orient c ->
                        let delta = Constant.PiTimesTwo * d * orientationFctr
                        let t = M44d.Rotation (m.camera.Right, -delta.Y) * M44d.Rotation (m.camera.Sky, -delta.X)

                        let newLocation = t.TransformDir (m.camera.Location)
                        let tempcam = m.camera.WithLocation newLocation
                        let newForward = c - newLocation |> Vec.normalize
                        let tempcam = tempcam.WithForward newForward
                               
                        { m with camera = CameraView.lookAt tempcam.Location c tempcam.Up}         
                    | Panning c ->
                        let step = (m.camera.Down * float d.Y + m.camera.Right * float d.X) * panningFctr
                        { m with camera = m.camera.WithLocation (m.camera.Location + step ); center = Some (c + step)}
                    | Zoom c ->
                        let step = (m.camera.Forward * float +d.Y) * -zoomingFctr
                        let newLoc = clampedLocation m.camera.Location step c
                        { m with camera = m.camera.WithLocation newLoc}
                    | NoOp -> m 
            | TimeStep dt -> 
                let dir = m.forward.X * m.camera.Right + m.forward.Y * m.camera.Forward
                let speed = dt * 0.01                
                { m with camera = m.camera.WithLocation(m.camera.Location + dir * speed )}
            | PickPoint c when m.picking.IsSome && pickEx -> 
                let newForward = c - m.camera.Location |> Vec.normalize
                let tempCam = m.camera.WithForward newForward
                { m with center = Some c; camera = CameraView.lookAt tempCam.Location c tempCam.Up }
            | _ -> m

    let subscriptions (time : IMod<DateTime>) (m : Model) =
        Many [                            
                
            Input.moveDelta MouseDelta                
            //Sub.time(TimeSpan.FromMilliseconds 30.0) ( fun a -> TimeStep 30.0)
            //Sub.ofMod time (fun _ ms -> [TimeStep ms])        
        ]
    
    let initial = { 
        camera = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
        //frustum = Frustum.perspective 60.0 0.01 10.0 (1024.0/768.0); 
        _id = null
        lookingAround = None
        panning = None
        zooming = None
        picking = None
        forward = V2d.OO
        forwardSpeed = 0.0
        center = Some V3d.Zero
        navigationMode = NavigationMode.FreeFly
        }

    let ofPickMsg _ m = []

    let app time : App<Model,MModel,Action,ISg<Action>> =
        {
            initial = initial
            update = update true
            view = view
            ofPickMsg = ofPickMsg 
            subscriptions = subscriptions time
        }

module FreeFlyCameraApp =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Scratch.DomainTypes2
    open CameraTest
    open Primitives
    open Aardvark.ImmutableSceneGraph.Scene
    open Input

    type Action = 
        | MouseDelta of V2d        
        | AddMove    of V2d
        | RemoveMove of V2d
        | Animate    of DateTime
        | PickPoint  of V3d        
        | TimeStep   of float

    let point = Mod.init V3d.Zero

    let view (m : MModel) =
        [Sphere (Sphere3d(V3d.OOO, 1.0))
            |> Scene.render []
         Sphere (Sphere3d(V3d.OOO, 0.05)) 
            |> Scene.render []             
            |> colored' (Mod.constant C4b.Red)
            |> Scene.transform' (point |> Mod.map (fun a -> Trafo3d.Translation (a)));]      
        |> Scene.group
        |> Scene.viewTrafo (m.mcamera |> Mod.map CameraView.viewTrafo)
        //|> Scene.projTrafo (m.mfrustum |> Mod.map Frustum.projTrafo)

    let forward = V2d.OI
    let backward = -V2d.OI
    let left = -V2d.IO
    let right = V2d.IO
    let clampDir (v : V2d) = V2d(clamp -1.0 1.0 v.X, clamp -1.0 1.0 v.Y)
    let orientationFctr = 1.0
    let panningFctr = 1.0
    let zoomingFctr = 1.0

    let update e (m : Model) msg = 
        match msg with            
            | MouseDelta d -> 
                match (m.lookingAround, m.panning, m.zooming) with
                    | Some _, None, None -> //orient
                        let delta = Constant.PiTimesTwo * d * orientationFctr
                        let t = M44d.Rotation (m.camera.Right, -delta.Y) * M44d.Rotation (m.camera.Sky, -delta.X)
                        let forward = t.TransformDir m.camera.Forward |> Vec.normalize
                        { m with camera = m.camera.WithForward forward }
                    | None, Some _, None -> //pan
                        let step = (m.camera.Down * float d.Y + m.camera.Right * float d.X) * panningFctr
                        { m with camera = m.camera.WithLocation (m.camera.Location + step )}
                    | None, None, Some _ -> //zoom
                        let step = (m.camera.Forward * float -d.Y) * zoomingFctr                        
                        { m with camera = m.camera.WithLocation (m.camera.Location + step)}
                    | _,_,_ -> m
            | AddMove d    -> { m with forward = clampDir <| m.forward + d }
            | RemoveMove d -> { m with forward = clampDir <| m.forward - d }
            | TimeStep dt -> 
                let dir = m.forward.X * m.camera.Right + m.forward.Y * m.camera.Forward
                let speed = dt * 0.01
                { m with camera = m.camera.WithLocation(m.camera.Location + dir * speed )}
//            | PickPoint p -> 
//                transact ( fun () -> Mod.change point p)
//                m       
            | _ -> m          

    let ofPickMsg _ m = []

    let subscriptions (time : IMod<DateTime>) (m : Model) =
        Many [
            
            Input.toggleKey Keys.W (fun _ -> AddMove forward)   (fun _ -> RemoveMove forward)
            Input.toggleKey Keys.S (fun _ -> AddMove backward)  (fun _ -> RemoveMove backward)
            Input.toggleKey Keys.A (fun _ -> AddMove left)      (fun _ -> RemoveMove left)
            Input.toggleKey Keys.D (fun _ -> AddMove right)     (fun _ -> RemoveMove right)                                   

            Input.moveDelta MouseDelta     

            //Input.key Direction.Down Keys.W (fun b a -> TimeStep 20.0)

            //Sub.time(TimeSpan.FromMilliseconds 25.0) ( fun a -> TimeStep 25.0)
            //Sub.ofMod time (fun t ms -> [TimeStep ms])
        ]

    let initial = { 
        camera = CameraView.lookAt (V3d.III * 3.0) V3d.OOO V3d.OOI
        ///frustum = Frustum.perspective 60.0 0.01 10.0 (1024.0/768.0); 
        _id = null
        lookingAround = None
        panning = None
        zooming = None
        picking = None
        forward = V2d.OO
        forwardSpeed = 0.0 
        center = None
        navigationMode = NavigationMode.FreeFly
        }

    let app time : App<Model,MModel,Action,ISg<Action>> =
        {
            initial = initial
            update = update
            view = view
            ofPickMsg = ofPickMsg 
            subscriptions = subscriptions time
        }

module ComposedTestApp = 
    open Aardvark.Base
    open Aardvark.Base.Rendering

    open Scratch.DomainTypes2
    open CameraTest
    open Primitives
    open Aardvark.ImmutableSceneGraph.Scene
    open ComposedTest

    open Input

    type Action = 
        | TranslateAction of TranslateController.Action
        | FreeFlyAction   of FreeFlyCameraApp.Action
        | OrbitAction     of OrbitCameraApp.Action        
        | DrawingAction   of SimpleDrawingApp.Action
        | DragStart       of PixelPosition
        | DragStop        of PixelPosition
        | PanStart        of PixelPosition
        | PanStop         of PixelPosition
        | ZoomStart       of PixelPosition
        | ZoomStop        of PixelPosition
        | PickStart  
        | PickStop   
        | SwitchMode        
        | SwitchInteraction
    
    let update e (m : ComposedTest.Model) msg =        
        let v = m.ViewerState
        let v = 
            match (msg, v.picking) with
                | (OrbitAction (OrbitCameraApp.Action.PickPoint _), Some _) when m.InteractionState = InteractionMode.ExplorePick ->                                 
                                { v with navigationMode = NavigationMode.Orbital }
                | _ -> v

        let v = 
            match msg with
                | DragStart p -> { v with lookingAround = Some p }
                | DragStop _  -> { v with lookingAround = None }
                | PanStart p  -> { v with panning = Some p }
                | PanStop _   -> { v with panning = None }
                | ZoomStart p -> { v with zooming = Some p }
                | ZoomStop _  -> { v with zooming = None }
                | PickStart   -> { v with picking = Some 0 }
                | PickStop    -> { v with picking = None }
                | FreeFlyAction a -> if v.navigationMode = NavigationMode.FreeFly && m.InteractionState <> InteractionMode.TrafoPick
                                     then FreeFlyCameraApp.update e v a else v
                | OrbitAction a   -> 
                            let explorePick = m.InteractionState = InteractionMode.ExplorePick
                            if v.navigationMode = NavigationMode.Orbital
                            then OrbitCameraApp.update explorePick e v a else v
                | DrawingAction _ -> v
                | SwitchMode -> 
                    match v.navigationMode with
                        | FreeFly -> 
                            let v' = { v with navigationMode = Orbital }
                            match v'.center with
                                | Some c ->  { v' with camera = v'.camera.WithForward (c - v'.camera.Location |> Vec.normalize)}
                                | None -> v'                        
                        | Orbital -> 
                            { v with navigationMode = FreeFly } 
                | _ -> v                

        let d = m.Drawing
        let d = 
            match msg with                
                | DrawingAction a -> 
                    if m.InteractionState = ComposedTest.InteractionMode.MeasurePick                       
                    then SimpleDrawingApp.update v.picking e d a else d
                | _ -> d

        let iState = 
            match msg with
                | SwitchInteraction ->
                    let s = match m.InteractionState with
                            | InteractionMode.ExplorePick -> InteractionMode.MeasurePick
                            | InteractionMode.MeasurePick -> InteractionMode.TrafoPick
                            | InteractionMode.TrafoPick -> InteractionMode.ExplorePick
                            | InteractionMode.Disabled -> m.InteractionState                                                
                    printfn "%A" s
                    s
                | _ -> m.InteractionState

        let t = m.Translation
        let t =
            match msg with
                | TranslateAction a -> if m.InteractionState = InteractionMode.TrafoPick 
                                       then TranslateController.updateModel e t a 
                                       else t
                | _ -> t
                    

       // printfn "%A %A" iState v.navigationMode
        { m with ViewerState = v; Translation = t; Drawing = d; InteractionState = iState }

    let ofPickMsgModel (m : Model) (pick : GlobalPick) = [
        yield! TranslateController.ofPickMsgModel m.Translation pick |> List.map (fun a -> TranslateAction a)
    ]       

    let subscriptions (time : IMod<DateTime>)  (m : ComposedTest.Model) =
        Many [      
            match m.ViewerState.navigationMode with
                | FreeFly -> yield FreeFlyCameraApp.subscriptions time m.ViewerState |> Sub.map FreeFlyAction                     
                | Orbital -> yield OrbitCameraApp.subscriptions time m.ViewerState |> Sub.map OrbitAction

            match m.InteractionState with
                | InteractionMode.MeasurePick -> 
                    yield SimpleDrawingApp.subscriptions m.Drawing |> Sub.map DrawingAction        
                | InteractionMode.TrafoPick -> ()
                | InteractionMode.Disabled | InteractionMode.ExplorePick -> ()
           
            yield Input.key Down Keys.N (fun _ _ -> SwitchMode)
            yield Input.key Down Keys.Space (fun _ _ -> SwitchInteraction)
            
            yield Input.toggleKey Keys.LeftCtrl (fun _ -> PickStart) (fun _ -> PickStop)

            yield Input.toggleMouse Mouse.left   DragStart  DragStop
            yield Input.toggleMouse Mouse.middle PanStart   PanStop
            yield Input.toggleMouse Mouse.right  ZoomStart  ZoomStop
        ]

    let viewTranslate m = TranslateController.viewModel m.mTranslation |> Scene.map TranslateAction

    // scene as parameter, isg 
    let view (frustum : IMod<Frustum>) (m : ComposedTest.MModel) : ISg<Action> =
        [
         // Sphere (Sphere3d(V3d.OOO, 1.0)) 
            Quad (Quad3d [| V3d(-2,-2,0); V3d(2,-2,0); V3d(2,2,0); V3d(-2,2,0) |])
                |> Scene.render [ on (Mouse.down' MouseButtons.Left) (OrbitAction   << OrbitCameraApp.PickPoint) 
                                  on  Mouse.move                     (DrawingAction << SimpleDrawingApp.MoveCursor) 
                                  on (Mouse.down' MouseButtons.Left) (DrawingAction << SimpleDrawingApp.AddPoint)
                                ]
                 
            OrbitCameraApp.viewCenter m.mViewerState |> Scene.map OrbitAction
            SimpleDrawingApp.view m.mDrawing |> Scene.map DrawingAction

            viewTranslate m
        ]
        |> Scene.group            
        |> Scene.camera (Mod.map2 Camera.create m.mViewerState.mcamera frustum) 
    
    let initial : ComposedTest.Model = { 
        _id = null
        ViewerState = FreeFlyCameraApp.initial
        Translation = TranslateController.initalModel
        Drawing = SimpleDrawingApp.initial
        InteractionState = ComposedTest.InteractionMode.TrafoPick       
        }

    let app time frustum = //: App<ComposedTest.Model,ComposedTest.MModel,Action,ISg<Action>> =
        {
            initial = initial
            update = update
            view = view frustum
            ofPickMsg = ofPickMsgModel // TranslateController.ofPickMsg 
            subscriptions = subscriptions time
        }