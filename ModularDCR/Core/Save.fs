module Deparse

open DCR

let deparse t G = 
    let outrel' printer = Map.iter (fun e -> Set.iter (printer e))
    let outrel r = outrel' (fun e f -> fprintfn t "\"%s\" %s \"%s\"" e r f) 
    
    G.resp |> outrel "*-->"
    G.cond_on |> outrel' (fun e f -> fprintfn t "\"%s\" -->* \"%s\"" f e)
    G.mile_on |> outrel' (fun e f -> fprintfn t "\"%s\" --<> \"%s\"" f e)
    G.excl |> outrel "-->%"
    G.incl |> outrel "-->+"

    G.events |> Set.iter (fun e -> 
        if Set.contains e G.local then fprintf t "/" 
        if not <| Set.contains e G.insi then fprintf t "%%" 
        if Set.contains e G.pend then fprintf t "!" 
        fprintf t "\"%s\"" e
        let l = Map.find e G.labels
        if e <> l then fprintf t "[%s]" l
        fprintfn t ""
        )




