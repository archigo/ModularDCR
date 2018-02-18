module Dot

open DCR
open D

type DcrDiagramOptions = { 
  direction        : Direction
  show_accept      : bool
  url0             : Option<string>
  intergroup_edges : bool
  intragroup_edges : bool
  max_nesting      : int
  escape           : bool
  execurl          : string -> string 
}  
and 
  Direction = LR | TB

let default_options_DCR = {
  direction        = TB
  show_accept      = true
  url0             = None
  intergroup_edges = true
  intragroup_edges = true
  max_nesting      = System.Int32.MaxValue
  escape           = true
  execurl          = fun _ -> ""
}

type NodeType = Cluster | Node

let e2id (e : string) = sprintf "__e%s" (e.Replace(" ", "-"))
let s2id' (h : int) = sprintf "__s%d" h
let s2id (G : DCR.dcr) = s2id' (G.GetHashCode())
let se2id' (h : int) (e : string) = sprintf "%s%s" (s2id' h) (e2id e)
let se2id (G : DCR.dcr) (e : string) = se2id' (G.GetHashCode()) e

let mk_dot options G t = 
    let dcr = G

    let excl_color = "grey65"

    (* Header. *)
    fprintfn t """digraph DCR {
                  margin=0.1;
                  penwidth=0.66;
                  rankdir=%s;
                  compound=true;
                  charset="UTF-8";
                  fontname="Roboto,Roboto-Regular";
                  fontsize="9";
               """
               // fontname="Helvetica neue";
               (match options.direction with 
                | LR -> "LR"
                | TB -> "TB")

    (* Acceptance label. *)
    if options.show_accept then
        fprintfn t """label = "(%s)";
                      labelloc=bottom;
                      labeljust=right;
                   """
                   (if accepting G then "Accepting" else "Not accepting")
                  
    (* Nodes, subgraphs. *)
    fprintfn t """node [shape=record,
                        fontsize="9",
                        tooltip="",
                        fontname="Roboto,Roboto-Regular"
                       ];
                       
                  edge [tooltip="",
                        fontname="Roboto,Roboto-Regular",
                        fontsize="9"];
               """

    let xs = executable G
    
    let idx e = 
        let events = dcr.events |> Seq.map (fun x -> (x, Node))
        let clusters = Grouping.names dcr.groups |> Seq.map (fun x -> (x, Cluster))
        let tab = Seq.append events clusters 
                  |> Seq.mapi (fun i (x, t) -> x, (i, t))
                  |> Map.ofSeq
        D.safeFind e tab

    let group_level x = 
      Seq.length (Grouping.ancestors G.groups x) - 1

    let prnode e = 
      if group_level e < options.max_nesting then
        let remote_url = Tags.tryFindUnique e "url" G
        let ifev = Tags.find e "external" G

        let style =
            [ (match Set.contains e G.insi, Set.contains e xs with
                 false, _ -> "" // "dotted"
               | _, false  -> "filled"
               | _        -> "")
              (if not <| Set.contains e G.local then "" else "rounded")
              (if remote_url <> None || ifev <> Set.empty then "dotted" else "")
            ] |> String.concat ","

        let label = 
            let l = D.safeFind e G.labels 
            [ 
              (match l with 
               | Neutral h -> h
               | Input h -> sprintf "%s?" h 
               | Output (h, expr) -> sprintf "%s<%s>" h <| Data.ppexpr expr
               | Computation (h, expr) -> sprintf "%s[[=%s]]" h <| Data.ppexpr expr)
              (if Set.contains e G.pend then "  !" else "")
              (if Set.contains e G.exec then "  &#10003;" else "")
              (match Map.tryFind e G.defs with Some _ -> "\n&#8862;" | None -> "")
            ] |> String.concat ""

        let color = 
            match Set.contains e G.pend, Set.contains e G.insi with
            | true, true -> "fontcolor=red3," 
            | _, false -> "fontcolor=" + excl_color + ","
            | _ -> ""

        let boundarycolor = 
            if not <| Set.contains e G.insi then "color=" + excl_color + "," else ""
        let url = 
            let u = match options.url0, remote_url with
                    | None, _       -> ""
                    | _, Some u'    -> u'
                    | Some u', None -> u'

            (*
            let ev = 
              if options.escape then
                System.Web.HttpUtility.UrlEncode e
              else
                e
            match Map.find e G.labels with
            | Input (h, parameters) as l when parameters <> [] -> 
                sprintf ",URL=\"javascript:exec_input_event('%s','%s','%s',%s)\"" 
                        <| u
                        <| e
                        <| DCR.ppLabel l
                        <| (parameters 
                            |> List.map (fun x -> sprintf "'%s'" x)
                            |> String.concat ","
                            |> sprintf "[%s]")
                      
            | _ -> *)
            sprintf ",URL=\"%s\"" (options.execurl e)

        let roles = Tags.find e "role" G |> String.concat ", "
        //let table = sprintf "shape=record,label=< <TABLE><TR><TD>%s</TD></TR><HR/><TR><TD>%s</TD></TR></TABLE> >" roles label
        //let table = sprintf "label=\"%s|\\n%s\\n \\n\"" roles label
        let table = 
          if roles = "" then 
            sprintf "shape=box,label=\"%s\"" label
          else 
            sprintf """shape=box,margin=0,label=< <TABLE BORDER="0" CELLBORDER="0" ROWS="*"><TR><TD CELLPADDING="2">%s</TD></TR><TR><TD CELLPADDING="12">%s</TD></TR></TABLE> >"""
              roles label

        fprintfn t "%d [id=\"%s\",%s,style=\"%s\",%s%sfillcolor=grey92%s];" 
                   (idx e |> fst)                                                 
                   (e2id e)
                   table
                   style
                   color
                   boundarycolor
                   url

    let cn = ref -1
    let rec subloop (Grouping.Partition es, k) = 
          if k < options.max_nesting then
            let once (name, es, g) = 
                let id = match name with
                         | Some x -> idx x |> fst 
                         | None -> (cn := !cn + 1; !cn)
                fprintfn t "subgraph cluster_%d {" id
                fprintfn t "label = \"%s\";" <| defaultArg name ""
                fprintfn t "style = dotted;"
                fprintfn t "margin = 14;"
                Set.iter prnode es
                subloop (g, k+1)
                // Dummy node for graphviz cluster-edge hack. 
                fprintfn t """_%d [shape="none",style="invisible",fixedsize="true",width=0,height=0,margin=0,label=""];""" id
                fprintfn t "}"
            es |> Set.iter once 
    subloop (G.groups, 0)

    let toplevel = 
        let rec loop (Grouping.Partition es) = 
            Set.fold (fun es (_, fs,g) -> Set.union es fs |> Set.union (loop g)) Set.empty es
        Set.difference G.events (loop G.groups) 

    Set.iter prnode toplevel

    (* Relations. *)
    let outrels0 printer = Map.iter (fun e -> Set.iter (printer e))
    let names = Grouping.names G.groups |> Set.ofSeq
    let outrel0 labeller e f = 
      let same_group = 
        let ancestors x = 
          Grouping.ancestors G.groups x |> Seq.filter ((<>) x) 
        List.ofSeq (ancestors e) = List.ofSeq (ancestors f)
      if ((options.intergroup_edges && not same_group) 
          || (options.intragroup_edges && same_group))
         && group_level e < options.max_nesting           
         && group_level f < options.max_nesting          
        then
        let idxe, te = idx e 
        let idxf, tf = idx f 
        let attrs = 
            seq {
                if not (Set.contains e names) && not (Set.contains e G.insi) then
                    yield sprintf "color=\"%s\"" excl_color
                    yield sprintf "labelfontcolor=\"%s\"" excl_color
                if te = Cluster then yield sprintf "ltail=cluster_%d" idxe
                if tf = Cluster then yield sprintf "lhead=cluster_%d" idxf
                match labeller e f with
                | Some l -> yield sprintf "label=\"%s\"" l
                | None -> ()
            }
            |> String.concat "," 

        fprintfn t "%s%d -> %s%d [%s];" 
                   (if te = Cluster then "_" else "") idxe
                   (if tf = Cluster then "_" else "") idxf
                   attrs

    let out_effects m =
      let labeller e f = 
        match Map.tryFind (e,f) m with
        | Some k when k = DCR.infinity -> None
        | Some k -> Some (string k)
        | None -> None
      outrel0 labeller |> outrels0
    
    let out_constraints constr = 
      let labeller e f = 
        match Map.tryFind (e,f) constr with
        | Some 0 -> None
        | Some k -> Some (string k)
        | None -> None
      outrel0 labeller |> D.flip |> outrels0 
    
    fprintfn t  "edge [arrowhead=normal, arrowtail=dot, dir=both];"
    dcr.resp |> out_effects G.t_r
    fprintfn t  "edge [arrowhead=dotnormal, arrowtail=none, dir=both];"
    dcr.cond_on |> out_constraints G.t_c
    fprintfn t  "edge [arrowhead=odiamond,arrowtail=none];"
    dcr.mile_on |> out_constraints Map.empty
    fprintfn t  "edge [arrowhead=none,arrowtail=none,headlabel=\"%%\"];"
    dcr.excl |> out_effects Map.empty
    fprintfn t  "edge [arrowhead=none,arrowtail=none,headlabel=\"+\"];"
    dcr.incl |> out_effects Map.empty

    fprintfn t  "}"

let compileDot format dotgen output = 
  use p = new System.Diagnostics.Process ()
  let pi = p.StartInfo
  pi.FileName <- 
    match System.Configuration.ConfigurationManager.AppSettings.["dot"] with 
    | null -> "dot"
    | x -> x
  pi.Arguments <- 
    match output with
    | None -> sprintf "-T%s" format 
    | Some file -> sprintf "-T%s -o%s" format file 
  pi.UseShellExecute <- false
  pi.RedirectStandardError <- true
  pi.RedirectStandardInput <- true
  if output = None then 
    pi.RedirectStandardOutput <- true
    pi.StandardOutputEncoding <- new System.Text.UTF8Encoding()
    
  p.Start() |> ignore
  dotgen p.StandardInput
  p.StandardInput.Close ()

  let err, out = 
    let err = async { 
      return Some <| p.StandardError.ReadToEnd () 
    }
    let out =
      match output with 
      | Some file -> async { return None }
      | None -> async { 
       return Some <| p.StandardOutput.ReadToEnd () }
    [| err; out |] 
    |> Async.Parallel
    |> Async.RunSynchronously
    |> fun x -> Option.get x.[0], x.[1]
       
  p.WaitForExit()
  if p.ExitCode <> 0 then
      error 0029 "Visualizer process failed"
  out

let compileDotFile format dotgen fname = 
  compileDot format dotgen (Some fname) |> ignore

let compileDotString format dotgen = 
  compileDot format dotgen None |> Option.get

