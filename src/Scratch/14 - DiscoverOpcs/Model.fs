namespace DiscoverOpcs.Model

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open DiscoverOpcs
open Adaptify
open Chiron

type Message = 
    | Restore
    | SetPaths of list<string>
    | Enter of int
    | Select of int
    | Discover
    | Save

[<ModelType>]
type Model = 
    {
        selectedPaths        : IndexList<string>
        opcPaths             : HashMap<string, list<string>>
        surfaceFolder        : list<string>
        bboxes               : list<Box2d>
        hover                : int
        highlightedFolders   : HashSet<string>
    }
    
    static member ToJson (m : Model) =
        json{
            do! Json.write "SelectedPath" (IndexList.toArray m.selectedPaths)
            do! Json.write "SelectedFolders" (HashSet.toArray m.highlightedFolders)
        }
    static member FromJson (_m : Model) =
        json{
            let! selectedPaths = Json.read "SelectedPath"
            let! selectedFolders = Json.read "SelectedFolders"

            return {
                    selectedPaths = IndexList.ofList selectedPaths
                    opcPaths = HashMap.Empty
                    surfaceFolder = List.Empty
                    bboxes = List.Empty
                    hover = -1
                    highlightedFolders = HashSet.ofList selectedFolders
                }
        }

    
