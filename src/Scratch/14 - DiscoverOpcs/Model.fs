namespace DiscoverOpcs.Model

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open DiscoverOpcs
open Adaptify

type Message = 
    | SetPaths of list<string>
    | Enter of int
    | Discover

[<ModelType>]
type Model = 
    {
        selectedPaths : IndexList<string>
        opcPaths      : HashMap<string, list<string>>
        surfaceFolder : list<string>
        bboxes        : list<Box2d>
        hover         : int
    }