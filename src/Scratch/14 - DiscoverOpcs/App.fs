﻿namespace DiscoverOpcs

open Aardvark.UI
open Aardvark.UI.Primitives
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open DiscoverOpcs.Model



module App =

  module Dialogs = 
    let onChooseFiles (chosen : list<string> -> 'msg) =
          let cb xs =
              match xs with
                  | [] -> chosen []
                  | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> List.map Aardvark.Service.PathUtils.ofUnixStyle |> chosen
                  | _ -> chosen []//failwithf "onChooseFiles: %A" xs
          onEvent "onchoose" [] cb   
  
  let importFolders (paths : list<string>) : list<OpcFolder> = 
    paths
      |> List.map(fun x ->
        if x |> Discover.isOpcFolder then x |> Opc
        elif x |> Discover.isSurface then x |> Surface
        elif x |> Discover.isSurfaceFolder then x |> SurfaceFolder
        else x |> Other
      )    

  let update (model : Model) (msg : Message) =
      match msg with
      | SetPaths paths -> 
        let selectedPaths = paths
        
        let opcPaths = 
          selectedPaths            
            |> List.map DiscoverOpcs.Discover.discoverOpcs 
            |> List.concat            

        { model with 
           selectedPaths = selectedPaths |> importFolders |> PList.ofList
           opcPaths = opcPaths |> PList.ofList 
        }
      | Discover -> failwith ""
          
  
  let folderText (folder:OpcFolder) =
    match folder with
    | SurfaceFolder s -> s
    | Surface s -> s
    | Opc s -> s
    | Other s -> s

  let createTag (folder:OpcFolder) =
      match folder with
      | SurfaceFolder _ -> div [clazz "ui middle aligned tiny label yellow"][text "SurfaceFolder"]
      | Surface       _ -> div [clazz "ui middle aligned tiny label orange"][text "Surface"]
      | Opc           _ -> div [clazz "ui middle aligned tiny label red"][text "Opc"]
      | Other         _ -> div [clazz "ui middle aligned tiny label blue"][text "SurfaceFolder"]

  let viewPaths (model:MModel) = 

    Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
      alist {
        for p in model.selectedPaths do
          yield div [clazz "ui inverted item"][              
              div [clazz "ui content"] [
                div [clazz "ui header tiny"] [p |> createTag; p |> folderText |> text]
              ]
            ]
      }
    )

  let viewOpcPaths (model:MModel) = 
    Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
      alist {
        for p in model.opcPaths do
          yield div [clazz "ui inverted item"][
              i [clazz "ui middle aligned box icon"] []
              div [clazz "ui content"] [
                div [clazz "ui header tiny"] [text p]                
              ]
            ]
      }
    )
  
  let view (model : MModel) =
    require Html.semui (
      body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
        div [clazz "ui inverted segment"] [
            h1 [clazz "ui"][text "Discover Opcs"]
            br []
            button [ 
              clazz "ui button tiny"
              Dialogs.onChooseFiles SetPaths;
              clientEvent "onclick" ("parent.aardvark.processEvent('__ID__', 'onchoose', parent.aardvark.dialog.showOpenDialog({properties: ['openDirectory', 'multiSelections']}));") ][
              text "Select Path"
            ]
            Html.SemUi.accordion "Paths" "files" true [viewPaths model]
                                
           // button [clazz "ui button tiny"; onClick (fun _ -> Discover)] [text "DiscoverOpcs" ]                
  
            Html.SemUi.accordion "Opcs" "boxes" true [viewOpcPaths model]
        ]
      ])
  
  
  let threads (model : Model) = 
      ThreadPool.empty
  
  
  let initPaths = @"I:\Dibit\1285_TSC_Selzthaltunnel_2017\Selzthaltunnel_RFB-Graz\2017_Bestandsaufnahme\OPC\OPC" |> List.singleton

  let opcPaths = 
    initPaths      
      |> List.map DiscoverOpcs.Discover.discoverOpcs 
      |> List.concat    

  let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      {
          unpersist = Unpersist.instance     
          threads = threads 
          initial = 
              { 
                 selectedPaths = initPaths |> importFolders  |> PList.ofList
                 opcPaths = opcPaths |> PList.ofList
              }
          update = update 
          view = view
      }
