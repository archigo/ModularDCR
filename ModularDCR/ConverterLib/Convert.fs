module Convert

open System

let raw (P : DCR.dcr) : string = 
    let mutable res = ""
    res <- sprintf "%d" <| Set.count P.events
    let events = P.events |> Array.ofSeq |> Array.mapi (fun idx x -> idx, x)
    for (_, ev) in events do 
        res <- res + Environment.NewLine + (sprintf "%d %d %d %s" 
            <| if Set.contains ev P.exec then 1 else 0
            <| if Set.contains ev P.insi then 1 else 0 
            <| if Set.contains ev P.pend then 1 else 0
            <| ev)

    let ids = 
      events 
        |> Seq.map (fun (idx, ev) -> (ev, idx))
        |> Map.ofSeq

    let rel src tgt typ = 
      let src = Map.find src ids 
      let tgt = Map.find tgt ids
      res <- res + Environment.NewLine + sprintf "%d %d %d" src tgt typ

    for (src, tgts) in Map.toSeq P.incl do 
      for tgt in tgts do 
        rel src tgt 0

    for (src, tgts) in Map.toSeq P.excl do 
      for tgt in tgts do 
        rel src tgt 1

    for (src, tgts) in Map.toSeq P.resp do 
      for tgt in tgts do 
        rel src tgt 2

    for (src, tgts) in Map.toSeq P.cond_on do 
      for tgt in tgts do 
        rel tgt src 3
    
    for (src, tgts) in Map.toSeq P.mile_on do 
      for tgt in tgts do 
        rel tgt src 4

    res

type Options = 
    { P : DCR.dcr
      outfile : System.IO.TextWriter
    }

let help _ = 
    printfn """Usage: $0 [options] [file]

    [-o | --output] <file>        Write output to <file>
    [-p | --parse] <string>       Parser input process from given string
"""


let convert (inp : string) : string = 
    let rec loop options = function        
        | "-o" :: file :: rest
        | "--output" :: file :: rest -> 
            let out = new System.IO.StreamWriter(file)
            loop { options with outfile = out } rest
        | "-p" :: str :: rest
        | "--parse" :: str :: rest -> 
            loop { options with P = Loader.loadstr str } rest           
        | file :: rest -> 
            loop { options with P = Loader.loadfile file } rest
        | [] -> 
            options
    let options = loop { P = DCR.empty; outfile = stdout } <| List.ofArray [| "-p"; inp |]

    let P = DCR.flatten options.P
    raw P