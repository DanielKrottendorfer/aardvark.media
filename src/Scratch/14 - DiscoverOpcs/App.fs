namespace DiscoverOpcs

open System
open System.IO

open Aardvark.UI
open Aardvark.UI.Primitives
  
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering
open DiscoverOpcs.Model
open Aardvark.SceneGraph.Opc
open PRo3D.Base


module App =
    let inline (==>) a b = Aardvark.UI.Attributes.attribute a b


    module Dialogs = 
        let onChooseFiles (chosen : list<string> -> 'msg) =
            let cb xs =
                match xs with
                | [] -> chosen []
                | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> List.map Aardvark.Service.PathUtils.ofUnixStyle |> chosen
                | _ -> chosen []//failwithf "onChooseFiles: %A" xs
            onEvent "onchoose" [] cb   
    
    //let importFolders (paths : list<string>) : list<OpcFolder> = 
    //  paths
    //    |> List.map(fun x ->
    //      if x |> Discover.isOpcFolder then x |> Opc
    //      elif x |> Discover.isSurface then x |> Surface
    //      elif x |> Discover.isSurfaceFolder then x |> SurfaceFolder
    //      else x |> Other
    //    )    
    
    let tryFileExists path = 
        if File.Exists path then Some path else None
    
    let tryDirectoryExists path = 
        if Directory.Exists path then Some path else None


    let scaleBoxes (boxes:List<Box3d>) (w:float) (h:float) = 
        let mutable min = boxes.Head.Min.XY
        let mutable max = boxes.Head.Max.XY

        for box in boxes do
            
            if box.X.Min < min.X then
                min.X <- box.X.Min
            if box.Y.Min < min.Y then
                min.Y <- box.Y.Min

            if box.X.Max > max.X then
                max.X <- box.X.Max
            if box.Y.Max > max.Y then
                max.Y <- box.Y.Max
        
        let positive_boxes = boxes |> List.map(fun box -> 
            Box2d(box.Min.XY - min,box.Max.XY - min)
        )
        
        let max = max-min

        let cropped_boxes = positive_boxes |> List.map( fun box -> 
            Box2d( (box.Min / max) , (box.Max / max) )
        )

        let scaled_boxes = cropped_boxes |> List.map( fun box ->

            let min = V2d(box.Min.X * w,box.Min.Y *h)
            let max = V2d(box.Max.X * w,box.Max.Y *h)
            Box2d(min ,max)
        
        )

        scaled_boxes
                
            
        

    let update (model : Model) (msg : Message) =
        match msg with
        | SetPaths paths -> 
            let selectedPaths = paths |> List.choose tryDirectoryExists
            
            Log.startTimed "Discovering Opcs"
            
            let opcs = 
                selectedPaths 
                |> List.map Discover.superDiscovery
                |> HashMap.ofList
            
            let surfacePaths = 
                selectedPaths
                |> List.map Discover.superDiscoveryMultipleSurfaceFolder
                |> List.concat
            
            let boxes = 
                surfacePaths 
                |> List.map(fun dir -> 

                    let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton
            
                    let patchHierarchies = [ 
                        for h in phDirs do
                        yield PatchHierarchy.load OpcSelectionViewer.Serialization.binarySerializer.Pickle OpcSelectionViewer.Serialization.binarySerializer.UnPickle (h |> OpcPaths)
                    ]
            
                    let box = 
                        patchHierarchies 
                        |> List.map(fun x -> x.tree |> QTree.getRoot) 
                        |> List.map(fun x -> x.info.GlobalBoundingBox)
                        |> List.fold (fun a b -> Box.Union(a, b)) Box3d.Invalid

                    box
                )
                |> List.map(fun box -> //transforming box to lon lat
                    let min = CooTransformation.getLatLonAlt box.Min Planet.Mars
                    let max = CooTransformation.getLatLonAlt box.Max Planet.Mars
                    
                    Box3d(V3d(min.latitude, min.longitude, min.altitude), V3d(max.latitude, max.longitude, max.altitude))                    
                )
            
            let bboxes = 
                if boxes.Length > 0 then 
                    let i = scaleBoxes boxes 800.0 600.0
                    printfn "%A" i
                    i
                else
                    List.empty

            let bboxes = bboxes |> List.map(fun box -> 
                Box2d(box.Min + V2d(5.0,5.0),box.Max + V2d(5.0,5.0))
            )

            printfn "%A" bboxes

            Log.stop()
            
            { model with 
               selectedPaths = selectedPaths |> IndexList.ofList
               opcPaths = opcs
               surfaceFolder = surfacePaths
               bboxes = bboxes
            }
        | Discover -> failwith ""
        | Enter i -> 
            printfn "%i" i
            { model with
                hover = i    
            }
            
    
    //let folderText (folder:OpcFolder) =
    //  match folder with
    //  | SurfaceFolder s -> s
    //  | Surface s -> s
    //  | Opc s -> s
    //  | Other s -> s
    
    //let createTag (folder:OpcFolder) =
    //    match folder with
    //    | SurfaceFolder _ -> div [clazz "ui middle aligned tiny label yellow"][text "SurfaceFolder"]
    //    | Surface       _ -> div [clazz "ui middle aligned tiny label orange"][text "Surface"]
    //    | Opc           _ -> div [clazz "ui middle aligned tiny label red"][text "Opc"]
    //    | Other         _ -> div [clazz "ui middle aligned tiny label blue"][text "Other"]
    
    let viewPaths (model:AdaptiveModel) = 
    
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                for p in model.selectedPaths do
                    yield div [clazz "ui inverted item"][              
                        div [clazz "ui content"] [
                            div [clazz "ui header tiny"] [p |> text]
                        ]
                    ]
            }
        )
    
    let viewOpcPaths (model:AdaptiveModel) = 
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                for (folder,opclist) in model.opcPaths |> AMap.toASet |> ASet.toAList do
                    //yield Html.SemUi.accordion "blub" "boxes" true [
                    yield h3 [][text (Path.GetFileName folder)]
                    for opc in opclist do
                        yield div [clazz "ui inverted item"][
                            i [clazz "ui middle aligned box icon"] []
                            div [clazz "ui content"] [
                              div [clazz "ui header tiny"] [text opc]      
                            ]
                        ]
                  //]
            }
        )
    
    let viewSurfacePaths (model:AdaptiveModel) = 
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                let! test = model.surfaceFolder
                let! hover = model.hover
                for i in 0..test.Length-1 do
                    if hover = i then
                        yield h3 [style "color: red";onMouseOver (fun _ -> Enter i)] [text (test.[i])]
                    else
                        yield h3 [style "color: white";onMouseOver (fun _ -> Enter i)] [text (test.[i])]
   
            }
        )
    
    let jsImportOPCDialog =
          "top.aardvark.dialog.showOpenDialog({tile: 'Select directory to discover OPCs and import', filters: [{ name: 'OPC (directories)'}], properties: ['openDirectory', 'multiSelections']}).then(result => {top.aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"
    
    let view (model : AdaptiveModel) =
    
        
        let drawBox (box : Box2d) attributes =      
            Svg.rect <| attributes @ [
                "x" ==> sprintf "%f"      box.Min.X
                "y" ==> sprintf "%f"      box.Min.Y
                "width" ==> sprintf  "%f" box.SizeX
                "height" ==> sprintf "%f" box.SizeY            
            ]

        let viewPolygon =
            alist {
                let! boxes = model.bboxes 
                let! hover = model.hover
                for i in 0..boxes.Length-1 do
                    if hover = i then
                        yield drawBox boxes.[i] [style "stroke:rgb(0,255,255);stroke-width:2;fill-opacity: .25;";onMouseOver (fun _ -> Enter i)]
                    else
                        yield drawBox boxes.[i] [style "stroke:rgb(255,255,255);stroke-width:2;fill-opacity: .25;";onMouseOver (fun _ -> Enter i)]
            }
    
        let svg =
            // read about svg elements here: https://www.w3schools.com/html/html5_svg.asp
            let attributes = 
                AttributeMap.ofList [
                    attribute "width" "820"; attribute "height" "620" 

                    // our event handlers require to search for the svg element to compute the coordinates realtive to.
                    // currently we use hard coded class name 'svgRoot' in our javascript code. see: aardvark.js
                    clazz "svgRoot"; 
                
                    // show a border for our svg
                    style "border: 5px solid white;"
                ]

            // finally create our svg. since our content is dynamic we use the incremental version of svg
            Incremental.Svg.svg attributes <| 
                alist {
                    yield! viewPolygon 
                }


        require Html.semui (
            body [style "width: 100%; height:100%; background: #252525; overflow-x: hidden; overflow-y: scroll"] [
                div [clazz "ui inverted segment"] [
                    h1 [clazz "ui"][text "Discover Opcs"]
                    br []
                    button [ 
                        clazz "ui button tiny"
                        Dialogs.onChooseFiles SetPaths;
                        clientEvent "onclick" (jsImportOPCDialog) ][
                        text "Select Path"
                    ]
                   // Html.SemUi.accordion "Paths" "files" true [viewPaths model]
                                        
                   // button [clazz "ui button tiny"; onClick (fun _ -> Discover)] [text "DiscoverOpcs" ]                
            
                    //Html.SemUi.accordion "Opcs" "boxes" true [viewOpcPaths model]
                    viewOpcPaths model
                ]
                div [clazz "ui inverted segment"] [
                    h1 [clazz "ui"][text "Discovered Surface Folder"]
                    br []
                    viewSurfacePaths model
                ]
                br []
                br []
                svg // here comes the actual svg
                br []
            ]
        )
    
    
    let threads (model : Model) = 
        ThreadPool.empty
        
    let initPaths = [] // @"G:\New_3D_Data\New_MSL_Data_jan_2018" |> List.singleton
    
    let opcPaths = 
        initPaths      
        |> List.map DiscoverOpcs.Discover.discoverOpcs 
        |> List.concat
    
    let initial =  { 
        selectedPaths = initPaths |> IndexList.ofList
        opcPaths = HashMap.empty //opcPaths |> IndexList.ofList
        surfaceFolder = List.empty
        bboxes = List.empty
        hover = -1
    }
    

    let app =

        {
            unpersist = Unpersist.instance     
            threads = threads 
            initial = initPaths |> SetPaths |> update initial
            update = update 
            view = view
        }
