﻿namespace Aardvark.UI

open System
open Suave

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI



module UI = 
    let onWheel (f : Aardvark.Base.V2d -> 'msg) =
        let serverClick (args : list<string>) : Aardvark.Base.V2d = 
            let delta = List.head args |> Pickler.unpickleOfJson
            delta  / Aardvark.Base.V2d(-100.0,-100.0) // up is down in mouse wheel events

        onEvent "onwheel" ["{ X: event.deltaX.toFixed(), Y: event.deltaY.toFixed()  }"] (serverClick >> f)

    let onWheel' (f : Aardvark.Base.V2d -> seq<'msg>) =
        let serverClick (args : list<string>) : Aardvark.Base.V2d = 
            let delta = List.head args |> Pickler.unpickleOfJson
            delta  / Aardvark.Base.V2d(-100.0,-100.0) // up is down in mouse wheel events

        onEvent' "onwheel" ["{ X: event.deltaX.toFixed(), Y: event.deltaY.toFixed()  }"] (serverClick >> f)

    let map (f : 'a -> 'b) (source : DomNode<'a>) : DomNode<'b> =
        source.Map f

module Combinators =
    let (=>) n v = attribute n v

open Combinators

type NumericInputType = Slider | InputBox

module List =
    /// The intersperse function takes an element and a list and
    /// 'intersperses' that element between the elements of the list.
    let intersperse sep ls =
        List.foldBack (fun x -> function
            | [] -> [x]
            | xs -> x::sep::xs) ls []

module Html =

    module Layout =
        let boxH ch = td [clazz "collapsing"; style "padding: 0px 5px 0px 0px"] ch

        let horizontal ch = table [clazz "ui table"; style "backgroundColor: transparent"] [ tbody [] [ tr [] ch ] ]

        let finish<'msg> = td[] []

    let ofC4b (c : C4b) = sprintf "rgb(%i,%i,%i)" c.R c.G c.B

    let table rows = table [clazz "ui celled striped inverted table unstackable"] [ tbody [] rows ]

    let row k v = tr [] [ td [clazz "collapsing"] [text k]; td [clazz "right aligned"] v ]

    type A = { a : IMod<int> }
    let a = Mod.init { a = Mod.init 10 }

    let test = 
        a |> Mod.map (fun z -> Mod.map (fun v -> v + 1) z.a)

    let semui =
        [ 
            { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
            { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]      

    module SemUi =
        open Aardvark.Base.AMD64.Compiler
        open Aardvark.Base.Geometry.RayHit
        
        
        let menu (c : string )(entries : list<string * list<DomNode<'msg>>>) =
            div [ clazz c ] (
                entries |> List.map (fun (name, children) ->
                    div [ clazz "item"] [ 
                        b [] [text name]
                        div [ clazz "menu" ] (
                            children |> List.map (fun c ->
                                div [clazz "item"] [c]
                            )
                        )
                    ]
                )
            )

        let adornerMenu (sectionsAndItems : list<string * list<DomNode<'msg>>>) (rest : list<DomNode<'msg>>) =
            let pushButton() = 
                div [
                    clazz "ui black big launch right attached fixed button menubutton"
                    js "onclick"        "$('.sidebar').sidebar('toggle');"
                ] [
                    i [clazz "content icon"] [] 
                    span [clazz "text"] [text "Menu"]
                ]
            [
                yield 
                    menu "ui vertical inverted sidebar menu" sectionsAndItems
                yield 
                    div [clazz "pusher"] [
                        yield pushButton()                    
                        yield! rest                    
                    ]
            ]                    

        let stuffStack (ls) =
            div [clazz "ui inverted segment"] [
                div [clazz "ui inverted relaxed divided list"] [
                    for l in ls do
                        yield
                            div [clazz "item"] [
                                div [clazz "content"] [
                                    l
                                ]
                            ]
                ]
            ]
        open Microsoft.FSharp.Reflection
    
        let private fields r =
            try 
                let t = r.GetType()
                let props = t.GetProperties()
                let vals = FSharpValue.GetRecordFields(r)
                [
                    for i in 0..props.Length-1 do
                        yield props.[i].Name, if props.[i].PropertyType = typeof<System.Double> then sprintf "%.2f" (vals.[i] :?> System.Double) else string vals.[i]
                ]
            with e -> []

        let recordPrint record =
            div [clazz "ui label"] [
                for (n,v) in fields record do
                    yield text (sprintf "%s: %s" n v)
            ]
            
        let accordion text' icon active content' =
            let title = if active then "title active inverted" else "title inverted"
            let content = if active then "content active" else "content"
            
            onBoot "$('#__ID__').accordion();" (
                div [clazz "ui inverted segment"] [
                    div [clazz "ui inverted accordion fluid"] [
                        div [clazz title] [
                                i [clazz (icon + " large icon circular")][] 
                                text text'
                                //Static.a [clazz "ui label"] [
                                //    i [clazz (icon + " icon circular inverted")] []
                                //    text text'
                                //]
                        ]
                        div [clazz content] content'
                    ]
                ]
            )

        let dropDown<'a, 'msg when 'a : enum<int> and 'a : equality> (selected : IMod<'a>) (change : 'a -> 'msg) =
            let names = Enum.GetNames(typeof<'a>)
            let values = Enum.GetValues(typeof<'a>) |> unbox<'a[]>
            let nv = Array.zip names values

            let attributes (name : string) (value : 'a) =
                AttributeMap.ofListCond [
                    always (attribute "value" name)
                    onlyWhen (Mod.map ((=) value) selected) (attribute "selected" "selected")
                ]

       //     onBoot "$('#__ID__').dropdown();" (
            select [onChange (fun str -> Enum.Parse(typeof<'a>, str) |> unbox<'a> |> change); style "color:black"] [
                for (name, value) in nv do
                    let att = attributes name value
                    yield Incremental.option att (AList.ofList [text name])
            ]
         //   )

        let textBox (text : IMod<string>) (set : string -> 'msg) =          
            
            let attributes = 
                amap {
                    yield "type" => "text"
                    yield onChange set
                    let! t = text
                    yield "value" => t 
                }

          //  div [clazz "ui input"] [
            Incremental.input (AttributeMap.ofAMap attributes)
            //]

        let toggleBox (state : IMod<bool>) (toggle : 'msg) =

            let attributes = 
                amap {
                     yield "type" => "checkbox"
                     yield onChange (fun _ -> toggle)

                     let! check = state
                     let checkText = if check then "checked" else ""
                     yield "checked" => checkText
                }

      //      div [clazz "ui toggle checkbox"] [
            Incremental.input (AttributeMap.ofAMap attributes)
        //        label [] [text ""]
            //]

        let toggleImage (state : IMod<bool>) (toggle : unit -> 'msg) = 0

        let tabbed attr content active =            
            onBoot "$('.menu .item').tab();" (
                div attr [
                    yield div [clazz "ui inverted segment top attached tabular menu"] [
                            for (name,ch) in content do
                                let active = if name = active then "inverted item active" else "inverted item"
                                yield Static.a [clazz active; attribute "data-tab" name][text name]
                          ]
                                        
                    for (name,ch) in content do
                        let classAttr = "ui inverted bottom attached tab segment"
                        let active = if name = active then (sprintf "%s %s" classAttr "active") else classAttr
                        yield div [clazz active; attribute "data-tab" name] [ch]         
                ]
            )

    module IO =

        let fileDialog action =
            [ 
                onEvent "onchoose" [] (List.head >> Aardvark.UI.Pickler.unpickleOfJson >> List.head >> action)
                clientEvent "onclick" ("aardvark.openFileDialog({ allowMultiple: true, mode: 'file' }, function(files) { if(files != undefined) aardvark.processEvent('__ID__', 'onchoose', files); });")
            ] 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
    open Aardvark.Base
    open Aardvark.UI

    type Model = Red=0 | Yellow=1 | Blue=2 

    type Action = Select of Model

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Numeric = 
    open Aardvark.Base
    open Aardvark.UI

    type Action = 
        | SetValue of float
        | SetMin of float
        | SetMax of float
        | SetStep of float
        | SetFormat of string

    let update (model : NumericInput) (action : Action) =
        match action with
        | SetValue v -> { model with value = v }
        | SetMin v ->   { model with min = v }
        | SetMax v ->   { model with max = v }
        | SetStep v ->  { model with step = v }
        | SetFormat s -> { model with format = s }

    let formatNumber (format : string) (value : float) =
        String.Format(Globalization.CultureInfo.InvariantCulture, format, value)

    let numericField<'msg> ( f : Action -> seq<'msg> ) ( atts : AttributeMap<'msg> ) ( model : MNumericInput ) inputType =         

        let tryParseAndClamp min max fallback s =
            let parsed = 0.0
            match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | (true,v) -> clamp min max v
                | _ ->  printfn "validation failed: %s" s
                        fallback

        let attributes = 
            amap {                
                yield style "text-align:right; color : black"                

                let! min = model.min
                let! max = model.max
                let! value = model.value
                match inputType with
                    | Slider ->   
                        yield "type" => "range"
                        yield onInput' (tryParseAndClamp min max value >> SetValue >> f)   // continous updates for slider
                    | InputBox -> 
                        yield "type" => "number"
                        yield onChange' (tryParseAndClamp min max value >> SetValue >> f)  // batch updates for input box (to let user type)

                let! step = model.step
                yield UI.onWheel' (fun d -> value + d.Y * step |> clamp min max |> SetValue |> f)

                yield "step" => sprintf "%f" step
                yield "min"  => sprintf "%f" min
                yield "max"  => sprintf "%f" max

                let! format = model.format
                yield "value" => formatNumber format value
            } 

        Incremental.input (AttributeMap.ofAMap attributes |> AttributeMap.union atts)

    let numericField' = numericField (Seq.singleton) AttributeMap.empty

    let view' (inputTypes : list<NumericInputType>) (model : MNumericInput) : DomNode<Action> =
        inputTypes 
            |> List.map (numericField' model) 
            |> List.intersperse (text " ") 
            |> div []

    let view (model : MNumericInput) =
        view' [InputBox] model

    let init = {
        value   = 0.0
        min     = 0.0
        max     = 10.0
        step    = 0.20
        format  = "{0:0.00}"
    }

    let app' inputTypes =
        {
            unpersist = Unpersist.instance
            threads = fun _ -> ThreadPool.empty
            initial = init
            update = update
            view = view' inputTypes
        }

    let app () = app' [NumericInputType.InputBox; NumericInputType.InputBox; NumericInputType.Slider]

    let start () =
        app () |> App.start 

module ColorPicker =
    type Action =
        | SetColor of ColorInput

    let spectrum =
        [   
            { kind = Stylesheet; name = "spectrumStyle"; url = "spectrum.css" }          
            { kind = Script; name = "spectrum"; url = "spectrum.js" }
        ]    

    let update (model : ColorInput) (action : Action) =
        match action with
            | SetColor c -> c

    let init = { c = C4b.VRVisGreen }

    let colorFromHex (hex:string) =
        Log.warn "%s" (hex.Replace("#", ""))
        let arr =
            hex.Replace("#", "")
                |> Seq.windowed 2
                |> Seq.mapi   (fun i j -> (i,j))
                |> Seq.filter (fun (i,j) -> i % 2=0)
                |> Seq.map    (fun (_,j) -> Byte.Parse(new System.String(j),System.Globalization.NumberStyles.AllowHexSpecifier))
                |> Array.ofSeq

        C4b(arr.[0], arr.[1], arr.[2], 255uy)

    let colorToHex (color : C4b) = 
        let bytes = [| color.R; color.G; color.B |]
        bytes 
            |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
            |> String.concat System.String.Empty

    let view (model : MColorInput) =
        require spectrum (
            onBoot "$('#__ID__').spectrum(
                        {
                            showPalette: true,
                            palette: [
                                ['#000','#444','#666','#999','#ccc','#eee','#f3f3f3','#fff'],
                                ['#f00','#f90','#ff0','#0f0','#0ff','#00f','#90f','#f0f'],
                                ['#f4cccc','#fce5cd','#fff2cc','#d9ead3','#d0e0e3','#cfe2f3','#d9d2e9','#ead1dc'],
                                ['#ea9999','#f9cb9c','#ffe599','#b6d7a8','#a2c4c9','#9fc5e8','#b4a7d6','#d5a6bd'],
                                ['#e06666','#f6b26b','#ffd966','#93c47d','#76a5af','#6fa8dc','#8e7cc3','#c27ba0'],
                                ['#c00','#e69138','#f1c232','#6aa84f','#45818e','#3d85c6','#674ea7','#a64d79'],
                                ['#900','#b45f06','#bf9000','#38761d','#134f5c','#0b5394','#351c75','#741b47'],
                                ['#600','#783f04','#7f6000','#274e13','#0c343d','#073763','#20124d','#4c1130']
                            ],
                            showSelectionPalette: true,
                            localStorageKey: 'spectrum.homepage',
                            preferredFormat: 'hex',
                            showInput: true
                            });" (
                let attributes = 
                    amap {                    
                        yield "type" => "text"
                        yield onChange (fun d -> { c = colorFromHex d }|> SetColor)

                        let! color = model.c
                        yield "value" => colorToHex color
                    }         

                Incremental.input (AttributeMap.ofAMap attributes)
        ))


    let viewSimple (color : IMod<C4b>) (change : C4b -> 'msg) =
        require spectrum (
            onBoot "$('#__ID__').spectrum(
                        {
                            showPalette: true,
                            palette: [
                                ['#000','#444','#666','#999','#ccc','#eee','#f3f3f3','#fff'],
                                ['#f00','#f90','#ff0','#0f0','#0ff','#00f','#90f','#f0f'],
                                ['#f4cccc','#fce5cd','#fff2cc','#d9ead3','#d0e0e3','#cfe2f3','#d9d2e9','#ead1dc'],
                                ['#ea9999','#f9cb9c','#ffe599','#b6d7a8','#a2c4c9','#9fc5e8','#b4a7d6','#d5a6bd'],
                                ['#e06666','#f6b26b','#ffd966','#93c47d','#76a5af','#6fa8dc','#8e7cc3','#c27ba0'],
                                ['#c00','#e69138','#f1c232','#6aa84f','#45818e','#3d85c6','#674ea7','#a64d79'],
                                ['#900','#b45f06','#bf9000','#38761d','#134f5c','#0b5394','#351c75','#741b47'],
                                ['#600','#783f04','#7f6000','#274e13','#0c343d','#073763','#20124d','#4c1130']
                            ],
                            showSelectionPalette: true,
                            localStorageKey: 'spectrum.homepage',
                            preferredFormat: 'hex',
                            showInput: true
                            });" (
                let attributes = 
                    amap {                    
                        yield "type" => "text"
                        yield onChange (change << colorFromHex)

                        let! color = color
                        yield "value" => colorToHex color
                    }         

                Incremental.input (AttributeMap.ofAMap attributes)
        ))

    let app : App<ColorInput, MColorInput, Action> =
        {
            unpersist = Unpersist.instance
            threads = fun _ -> ThreadPool.empty
            initial = init
            update = update
            view = view
        }

    let start () =
        app |> App.start 

module Vector3d = 
    open Aardvark.UI

    type Action = 
        | SetX of Numeric.Action
        | SetY of Numeric.Action
        | SetZ of Numeric.Action
        | SetXYZ of Numeric.Action * Numeric.Action * Numeric.Action
   
    let update (model : V3dInput) (action : Action) =
        match action with
            | SetX a -> 
                let x = Numeric.update model.x a                
                {                     
                    model with 
                        x = x
                        value = V3d(x.value, model.value.Y, model.value.Z)
                }
            | SetY a -> 
                let y = Numeric.update model.y a                
                {                     
                    model with 
                        y = y
                        value = V3d(model.value.X, y.value, model.value.Z)
                }
            | SetZ a -> 
                let z = Numeric.update model.z a                
                {                     
                    model with 
                        z = z
                        value = V3d(model.value.X, model.value.Y, z.value)
                }
            | SetXYZ (a,b,c) -> 
                let x = Numeric.update model.x a
                let y = Numeric.update model.y b
                let z = Numeric.update model.z c
                {                     
                    model with 
                        x = x
                        y = y
                        z = z
                        value = V3d(x.value, y.value, z.value)
                }
                
    let view (model : MV3dInput) =  
        
        Html.table [                            
            Html.row "X" [Numeric.view' [InputBox] model.x |> UI.map SetX]
            Html.row "Y" [Numeric.view' [InputBox] model.y |> UI.map SetY]
            Html.row "Z" [Numeric.view' [InputBox] model.z |> UI.map SetZ]
        ]                    

    let init = 
        let x = Numeric.init
        let y = Numeric.init
        let z = Numeric.init
    
        {
            x = x
            y = y
            z = z
            value = V3d(x.value,y.value,z.value)
        }        

    let initV3d (v : V3d) = {
        x = { Numeric.init with value = v.X } 
        y = { Numeric.init with value = v.Y }
        z = { Numeric.init with value = v.Z }
        value = v
    }

    let updateV3d (model : V3dInput) (v : V3d) = {
        x = { model.x with value = v.X } 
        y = { model.y with value = v.Y }
        z = { model.z with value = v.Z }
        value = v
    }

    let app : App<V3dInput, MV3dInput, Action> =
        {
            unpersist = Unpersist.instance
            threads = fun _ -> ThreadPool.empty
            initial = init
            update = update
            view = view
        }

    let start () =
        app |> App.start 

module TreeView = 
    
    type Action<'id> = 
        | Click of 'id

    let view attribs children = Incremental.div (AttributeMap.ofList [clazz "ui list"]) children

    let leaf click content dragStartMsg dragStopMsg =
        let dragStart = onEvent "ondragstart" ["event.target.id"] (fun xs -> printfn "start: %A" xs; dragStartMsg)
        let dragOver = js "ondragover" "console.warn('urdar'); event.preventDefault()"
        let dragStop = js "ondrop" "console.warn('bla'); event.preventDefault()" //onEvent "ondrop" ["event.preventDefault(); event.target.id"] (fun xs -> printfn "stop %A" xs; dragStartMsg)
        div [ clazz "item"; onMouseClick (fun _ -> click ()); dragStart; dragOver; dragStop; "draggable" => "true"] [
            i [ clazz "file icon";  ] []
            Incremental.div (AttributeMap.ofList [clazz "content" ]) content
        ]

    let node (isExpanded : IMod<bool>) (clickMsg : unit -> 'a) header description children =
        let itemAttributes =
            amap {
                yield onMouseClick (fun _ -> clickMsg ())
                let! selected = isExpanded
                if selected then yield clazz "icon large outline open folder"
                else             yield clazz "icon large outline folder"
            } |> AttributeMap.ofAMap

        let childrenAttribs =
            amap {
                yield clazz "list"
                let! isExpanded = isExpanded
                if isExpanded then yield style "visible"
                else yield style "hidden"
            }

        div [ clazz "item" ] [
             Incremental.i itemAttributes AList.empty
             div [ clazz "content" ] [
                 div [ clazz "header"] [header]
                 div [ clazz "description noselect"] [description]
                 Incremental.div (AttributeMap.ofAMap childrenAttribs) 
                    <| alist { 
                        let! isExpanded = isExpanded
                        if isExpanded then yield! children
                    }
             ]
        ]

module TreeViewApp =
    
    open TreeView

    type Action =
        | Click of list<Index>
        | ToggleExpand of list<Index>
        | AddChild of list<Index>
        | RemChild of list<Index>
        | Nop

    let click v () = Click v
    let toggle v () = ToggleExpand v
    let addChild v () = AddChild v
    let remChild v () = RemChild v

    let defaultP = { isExpanded = true; isSelected = false; isActive = false }

    let init =
        { data =
            Tree.node (LeafValue.Text "0") defaultP <| PList.ofList [ 
                Leaf (LeafValue.Number 1)
                Leaf (LeafValue.Text "2" )
                Tree.node (LeafValue.Number 3) defaultP <| PList.ofList [
                    yield Leaf (LeafValue.Number 4)
                    yield Leaf (LeafValue.Number 5) 
                ]
            ] 
        }

    let updateAt (p : list<Index>) (f : Tree -> Tree) (t : Tree) =
        let rec go (p : list<Index>) (t : Tree)  =
            match p with
                | [] -> f t
                | x::rest -> 
                    match t with
                        | Leaf _ -> t
                        | Node(l,p,xs) -> 
                            match PList.tryGet x xs with
                                | Some c -> Node(l,p, PList.set x (go rest c) xs)
                                | None   -> t
        go (List.rev p) t

    let update (model : TreeModel) action =
        printfn "action: %A" action
        match action with
            | Click p ->                 
                { model with
                    data = updateAt p (function | Leaf v ->( match v with 
                                                                | LeafValue.Number n -> Leaf ( LeafValue.Number (n + 1))
                                                                | LeafValue.Text t -> Leaf ( LeafValue.Text (sprintf "%s a" t)))
                                                | p -> p) model.data
                }
            | ToggleExpand p -> 
                { model with
                    data = 
                        updateAt p (
                            function | Leaf v -> Leaf v
                                     | Node(l,p,xs) -> 
                                         Node(l, { p with isExpanded = not p.isExpanded}, xs)
                        ) model.data
                }
            | AddChild p -> 
                { model with
                    data = updateAt p (
                             function | Leaf v -> Leaf v
                                      | Node(l,p,xs) -> 
                                            let value = match l with
                                                           | Number n -> Number (PList.count xs + 1)
                                                           | Text   t -> LeafValue.Text t
                                            Node(l,p, PList.append (Leaf value) xs)
                           ) model.data
                }
            | RemChild p -> 
                { model with
                    data = updateAt p (
                             function | Leaf v -> Leaf v
                                      | Node(l,p,xs) -> 
                                          Node(l,p, if PList.count xs > 0 then PList.removeAt 0 xs else xs)
                           ) model.data
                }
            | Nop -> model
    
    let viewLabel v = 
        v |> Mod.bind (fun u -> match u with 
                                    | MNumber n -> n |> Mod.map (fun x -> sprintf "Number %A" (string x))
                                    | MText t   -> t |> Mod.map (fun x -> sprintf "Text %A" x))
        |> Incremental.text
                        

    let rec viewTree path (model : IMod<MTree>) =
        alist {
            let! model = model
            match model with
            | MLeaf v -> 
                yield TreeView.leaf (click path) (AList.ofList [viewLabel v]) Nop Nop
            | MNode(s, p, xs) -> 
                let children = AList.collecti (fun i v -> viewTree (i::path) v) xs
                let desc =
                    div [] [
                         i [ clazz "plus icon";  onClick (addChild path) ] []
                         i [ clazz "minus icon"; onClick (remChild path) ] []
                    ]
                yield TreeView.node p.isExpanded (toggle path) 
                                    (viewLabel s) desc
                                    children
        }

    let view (model : MTreeModel) =
        require Html.semui (
            TreeView.view [] (viewTree [] model.data)
        )

    let app =
        {
            unpersist =  Unpersist.instance
            threads = fun _ -> ThreadPool.empty
            initial = init
            update = update
            view = view 
        }

    let start () =
        app |> App.start 