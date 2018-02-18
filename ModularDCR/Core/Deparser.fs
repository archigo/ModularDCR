module Deparser

open D

let rec loop t indent (G : DCR.dcr) = 
    let out' x = fprintf t x
    let outp i = out' "%s" <| String.replicate (i+indent) "  "
    let out = outp 0 ; fprintfn t  

    out "# Events"

    let outlab' e = 
        out' "\"%s\"" e
        match DCR.labelOf G e with
        | DCR.Input (str) -> 
            out' "[\"%s\"]?)" str
        | DCR.Output (str, expr) ->
            out' "[\"%s\"]<%s>"
            <| str 
            <| failwithf "Deparsing of output data not implented."
            //<| (exprs |> Seq.map (fun (v,e) -> Data.ppexpr |> String.concat ", ")
        | DCR.Neutral str -> 
            out' "[\"%s\"]" str
        | DCR.Computation (str, expr) -> 
            failwithf "Deparsing of computation expression not implemented."

    for e in G.events do 
        outp 0 
        if Set.contains e G.local then out' "/"
        if Set.contains e G.insi |> not then out' "%%"
        if Set.contains e G.pend then out' "!"
        if Set.contains e G.exec then out' ":"
        out' "\"%s\"" e
        match DCR.labelOf G e with
        | DCR.Neutral s -> s
        | DCR.Output (s, _) -> s
        | DCR.Input (s) -> s
        | _ -> failwithf "Deparsing of computation expression not implemented."
        |> out' "[\"%s\"" 

        let tags = Map.tryFind e G.tags |> flip defaultArg Map.empty |> Map.toSeq
        if not <| Seq.isEmpty tags then 
            for (key, values) in tags do
                for v in values do 
                    out' " %s = \"%s\"" key v  

        out' "]"
        match Map.find e G.labels with 
        | DCR.Output (_, exprs) -> 
            failwithf "Deparsing output labels not implemented."
            //exprs |> Seq.map Data.ppexpr |> String.concat ", " |> out' "<%s>" 
        | _ -> ()

        out' "\n"

    out ""

    let subs = G.defs |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    if Seq.isEmpty subs |> not then out "# Sub-processes"

    for e in subs do
        for def in Map.find e G.defs do
            outlab' e
            out " {"
            loop t (indent+1) def
            out "}"
        if Map.find e G.defs = Set.empty then 
            outlab' e
            out "{ }"

    if Seq.isEmpty subs |> not then out ""

    let rec dogroup i (Grouping.Partition ps as P) = 
        for (name, events, p) in ps do
            outp i; out' "GROUP"
            match name with 
            | Some x -> out' " \"%s\"" x
            | None -> () 
            out' " {\n" 
            let handled = dogroup (i+1) p
            outp (i+1)
            Set.difference events handled |> Set.iter (out' "\"%s\" ")
            out' "\n"; outp i; out' "}\n"
        Grouping.domain P
    dogroup 0 G.groups |> ignore

    let dorel r rstr inv = 
        for (e, fs) in r |> Map.toSeq do 
            let left = sprintf "\"%s\"" e
            let right = 
                fs
                |> Seq.map (sprintf "\"%s\"")
                |> String.concat " "
                |> sprintf "(%s)"
            let left,right = if inv then right,left else left,right
            out' "%s %s %s\n" left rstr right

    dorel G.cond_on "-->*" true
    dorel G.mile_on "--<>" true
    dorel G.excl    "-->%" false
    dorel G.incl    "-->+" false
    dorel G.resp    "*-->" false

let deparse t = loop t 0 

let deparse' G = 
  use wr = new System.IO.StringWriter()
  deparse wr G
  wr.ToString()
  
