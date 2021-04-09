namespace DiscoverOpcs.Model

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open DiscoverOpcs
open Adaptify
open Chiron

type Message = 
    | SetPaths of list<string>
    | Enter of int
    | Select of int
    | Discover
    | Save

[<ModelType>]
type Model = 
    {
        selectedPaths : IndexList<string>
        opcPaths      : HashMap<string, list<string>>
        surfaceFolder : list<string>
        bboxes        : list<Box2d>
        hover         : int
        selectedFolders      : HashSet<string>
    }

type Selected =
    {
        selected : string list
    }

    static member ToJson (s : Selected) =
        json{
            do! Json.write "Selected" s.selected
        }

    static member FromJson (_s : Selected) =
        json{
            let! selected = Json.read "Selected"
            return {selected = selected}
        }
    
