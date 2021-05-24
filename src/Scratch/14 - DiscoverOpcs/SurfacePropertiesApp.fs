namespace DiscoverOpcs

open System
open System.IO

open Aardvark.UI
open Aardvark.UI.Primitives
  
open Aardvark.Base
open Aardvark.UI
open FSharp.Data.Adaptive
open DiscoverOpcs.Model
open System.Diagnostics

module SurfacePropertiesApp =
    
    let update (model : OpcSurface) (action : SurfacePropertiesAction) : OpcSurface =
        match action with
        | Nop -> model        
        | OpenExplorer -> 
            let path = model.path
            Process.Start("explorer.exe", path ) |> ignore
            model

    let properties (model : AdaptiveOpcSurface) =                         
        div [clazz "ui inverted item"][
            div [clazz "ui item"][Incremental.text model.filename]
            div [clazz "ui item"][Incremental.text model.path]
            div [clazz "ui item"][Incremental.text (model.bounds |> AVal.map(fun x -> x.ToString())) ] //aval<Box3d> -> aval<string>  ... x<`a> -> x<`b>
        ]                    

    let view (model : AdaptiveOpcSurface) : DomNode<SurfacePropertiesAction> =
        div [clazz "ui inverted segment"] [
            h1 [clazz "ui"][text "Properties"]
            properties model
            button[onClick(fun _ -> OpenExplorer) ][text "Open Folder"]
        ]

