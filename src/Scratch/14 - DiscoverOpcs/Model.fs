namespace DiscoverOpcs.Model

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open DiscoverOpcs
open Adaptify
open Chiron

module ui =
    let dockConfig = 
      config {
          content (
              horizontal 10.0 [
                
                  vertical 10.0 [
                      element { id "files"; title "Files"; weight 50; isCloseable false }
                      element { id "properties"; title "Properties"; weight 50; isCloseable false }
                  ]
                  vertical 50.0 [
                      element { id "boxes"; title "Box View"; weight 10; isCloseable false }
                  ]
                  vertical 10.0 [
                      element { id "scene"; title "Scene"; weight 50; isCloseable false }
                      element { id "3d_preview"; title "3D Preview"; weight 50; isCloseable false }
                  ]
              ]
          )
          appName "Pro3D Repository"
          useCachedConfig false
      }

type SurfacePropertiesAction =
    | OpenExplorer
    | Nop

type Message = 
    | Restore
    | SetPaths of list<string>
    | Enter of int
    | Select of int
    | Discover
    | Save
    | UpdateConfig of DockConfig
    | UpdateProperties of int    
    | SurfacePropertiesMessage of SurfacePropertiesAction

[<ModelType>]
type OpcSurface = {
    filename    : string
    path        : string
    bounds      : Box3d
}


[<ModelType>]
type Model = 
    {
        selectedPaths        : List<string>
        lastSelectedPath     : string
        opcPaths             : HashMap<string, list<string>>
        surfaceFolders       : list<string>
        bboxes               : list<Box2d>
        surfaces             : list<OpcSurface>
        selectedSurface      : OpcSurface
        hover                : int
        highlightedFolders   : HashSet<string>
        dockConfig           : DockConfig
    }

    static member Default = 
      { 
          selectedPaths = List.Empty
          lastSelectedPath = ""
          opcPaths = HashMap.empty //opcPaths |> IndexList.ofList
          selectedSurface = {
              filename = ""
              path = ""
              bounds = Box3d()
          }
          surfaces = List.empty
          surfaceFolders = List.empty
          bboxes = List.empty
          hover = -1
          highlightedFolders = HashSet.empty
          dockConfig = ui.dockConfig
      }
    
    static member ToJson (m : Model) =
        json{
            do! Json.write "SelectedPath" (m.selectedPaths)
            do! Json.write "HighlightedFolders" (HashSet.toArray m.highlightedFolders)
        }
    static member FromJson (_m : Model) =
    
        let def = Model.Default
        json{
            let! selectedPaths = Json.read "SelectedPath"
            let! highlightedFolders = Json.read "HighlightedFolders"
            return {
                  selectedPaths = selectedPaths
                  lastSelectedPath = ""
                  selectedSurface = {
                      filename = ""
                      path = ""
                      bounds = Box3d()
                  }
                  opcPaths = HashMap.Empty
                  surfaceFolders = List.Empty
                  bboxes = List.Empty
                  hover = -1
                  highlightedFolders = HashSet.ofList highlightedFolders
                  dockConfig = ui.dockConfig
                  surfaces = List.empty
                }
            }
        