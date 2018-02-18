module ModelChecking

open D
open TS

// Events that are conditions for other events 
// and which are not permanently excluded. 
// NB! "Permanently excluded" is not stable under un-safe merge. 
let relevant_for_history (G : DCR.dcr) = 
  let includes = 
    G.incl
    |> Map.toSeq
    |> Seq.collect (snd >> Seq.collect (Grouping.descendants G.groups))
    |> Set.ofSeq

  let conditions = 
    G.cond_on 
    |> Map.toSeq 
    |> Seq.collect (snd >> Seq.collect (Grouping.descendants G.groups))
    |> Set.ofSeq

  conditions 
  |> Set.filter (fun e -> 
       not (not (Set.contains e G.insi) && not (Set.contains e includes)))


let trim e (G : DCR.dcr) = 
  if Set.contains e <| relevant_for_history G then
    G
  else
    { G with DCR.dcr.exec = Set.remove e G.exec }
  
  (*
  let r = relevant G |> Set.union G.exec
  fun e G' -> 
    if not <| Set.contains e r then
      { G' with DCR.dcr.exec = Set.remove e G'.exec }
    else
      G'
*)

let trimmed_exec G e = DCR.execute e G |> trim e
    //fun G e -> DCR.execute e G |> trim G e

let ts G = {
    actions = DCR.executable 
    move    = trimmed_exec
    state0  = G
    gen     = Set.toSeq
}


// Determine if the finite language of G restricted to the labels of H 
// is a subset of the finite language of H.
let check_projected_sublang G H =
    if DCR.has_subprocesses G || DCR.has_subprocesses H then 
        error 0023 "Can't model-check DCR graphs with sub-processes."
    if not <| DCR.is_definite H then
        error 0024 "Can only compare language inclusion for definite DCR graphs."

    let labels_H = H.labels |> Map.toSeq |> Seq.map snd |> Set.ofSeq
    let labels_G = G.labels |> Map.toSeq |> Seq.map snd |> Set.ofSeq

    if not <| Set.isSubset labels_H labels_G then
        error 0025 "Specification graph cannot have private labels."

    let event_at_label l = 
        let table = 
            H.labels |> Map.fold (fun table e l -> Map.add l e table) Map.empty 
        Map.tryFind l table

    // Trim away events not conditions for anything from marking. 
    // This is only valid when G.cond_on is constant throughout search.
    // For basic DCR graphs, that is the case.
    let exec_G e G = trimmed_exec G e
    let exec_H e H = trimmed_exec H e

    let transitions = function 
        | (G, Some H) -> 
            seq { 
                let hs = DCR.executable H
                let gs = DCR.executable G
                for e in gs do
                    let G' = exec_G e G
                    let l = Map.find e G.labels
                    match event_at_label l with
                    | None -> yield (l, G', Some H)
                    | Some f -> 
                        let corr = 
                            if hs |> Set.contains f then
                                Some <| exec_H f H
                            else
                                None
                        yield (l, G', corr)
            } |> Seq.cache

        | (G, None) -> 
            DCR.executable G 
            |> Seq.map (fun e -> Map.find e G.labels, exec_G e G, None)
            |> Seq.cache
                
        
    let move _ (_, G, optH) = (G, optH)

    let P state _ = 
        match state with 
        | G, None -> DCR.accepting G
        | G, Some H -> DCR.accepting G && not (DCR.accepting H)

    let ts = { 
        actions = transitions
        move = move
        state0 = G, Some H 
        gen = id
    }

    search 2 ts P 

let verify_simple G = 
   if DCR.has_subprocesses G then
       error 0026 <| "Can't exhaustively model-check DCR graphs with subprocesses." 

let check P (G : DCR.dcr) =
   verify_simple G
   let no_threads = System.Environment.ProcessorCount * 2
   search no_threads (ts G) P

#nowarn "193"   // Yes, it's points-free.

let check_event e G = 
  DCR.verify_event G e
  check (fun _ x -> Set.contains e x) G

let check_event_path e hit avoid G = 
  [Set.singleton e; hit; avoid] |> Set.unionMany |> Set.iter (DCR.verify_event G)
  verify_simple G
  let move = trimmed_exec
  let ts = { 
      actions = fun (H, _) -> Set.difference (DCR.executable H) avoid 
      move    = fun (H, missing) e -> move H e, Set.remove e missing
      state0  = G, hit
      gen     = Set.toSeq
  }
  let P (H, missing) x = Set.contains e x && missing = Set.empty

  search 2 ts P 

let check_termination = 
  check <| fun G x -> Set.isEmpty x && DCR.accepting G

let check_acceptance = 
  check <| fun G _ -> DCR.accepting G

let check_deadlock = 
  check <| fun G x -> Set.isEmpty x && not <| DCR.accepting G

let check_not_live = 
  check <| fun G _ -> 
    match check_acceptance G with
    | Some _, _ -> false
    | None, _ -> true

let count = check (fun _ _ -> false) >> snd

type traversal = (DCR.dcr -> Set<DCR.event> -> unit) -> DCR.dcr -> unit

type state = Preamble | Entry | Body | Exit

let traverse_subgraph entry exit hit avoid f G = 
  // TODO: missing currently doesn't do anything
  [entry; exit; hit; avoid] 
  |> Seq.iter (Seq.iter <| DCR.verify_event G)
  verify_simple G

  let ts = { 
    actions = function 
      | (_, _, Exit) -> Set.empty
      | (H, _, Entry) -> Set.intersect (DCR.executable H) entry
      | (H, _, Preamble) -> DCR.executable H
      | (H, _, Body) -> Set.difference (DCR.executable H) avoid 
    move = fun (H, missing, state) e -> 
      let H' = trimmed_exec H e
      H',
      Set.remove e missing, 
      match state with
      | Entry -> 
          if Set.contains e entry then Body 
          elif Set.empty <> Set.intersect (DCR.executable H') entry then Entry
          else Preamble
      | Preamble -> 
          if Set.empty <> Set.intersect (DCR.executable H') entry then 
            Entry 
          else 
            Preamble
      | Body -> 
          if Set.contains e exit then Exit else Body
      | Exit -> failwith "Moved from exit."
    state0  = G, hit, Preamble
    gen     = Set.toSeq
  }

  let f' (H, _, state) xs = 
    match state with 
    | Entry | Body | Exit -> f H xs
    | _ -> ()
    false
    
  search 1 ts f' |> ignore

let traverse_all f G = 
  verify_simple G
  search 1 (ts G) (fun H xs -> f H xs ; false) |> ignore




let mk_tsr_dot_ traversal G t = 

  fprintfn t "digraph TSR {"
  fprintfn t "rankdir=LR"
  fprintfn t "node [shape=box,fontname=\"Roboto Mono\",fontsize=9];"
  fprintfn t "edge [fontname=\"Roboto Mono\",fontsize=9];"

  let f (G : DCR.dcr) x = 
    let n = G.GetHashCode()
    let label = 
      G.events 
      |> Seq.map (fun e -> 
          e + " "
            + if Set.contains e G.insi then "+" else "%"
            + if Set.contains e G.pend then "!" else " "
            + if Set.contains e G.exec then "." else " "
        )
      |> String.concat "\\r"
      |> (fun s -> s + "\\r")
    fprintfn t "%u [id=\"%s\",label=\"%s\"];" n (Dot.s2id' n) label

    let externs, interns = 
      x
      |> List.ofSeq
      |> List.partition (fun e -> 
        Tags.hasTag e "external" G)

    let externs = Seq.groupBy (trimmed_exec G) externs
    let interns = Seq.groupBy (trimmed_exec G) interns

    let iter style groups = 
      for (H, es) in groups do 
        let h = H.GetHashCode ()
        let id = 
          if Seq.length es = 1 then 
            sprintf "id=\"%s\"," (Seq.item 0 es |> Dot.se2id' n)
          else
            ""
        es 
        |> Seq.map (fun e -> Map.find e G.labels |> DCR.label_head)
        |> String.concat ","
        |> fprintfn t "%u -> %u [%s%slabel=\"%s\"]" n h id style

    iter "" interns
    iter "style=dotted," externs

  traversal f G 
  fprintfn t "}"

let mk_tsr_dot G t = mk_tsr_dot_ traverse_all G t

let emit_activity_box (G : DCR.dcr) e id t = 
  let label = Map.find e G.labels |> DCR.label_head
  let role = Tags.find e "role" G |> String.concat ","
  fprintfn t 
    "%u [id=\"%s\",shape=box%s,fontname=\"Roboto Mono\",fontsize=9,label=\"%s\n(%s)\"];" 
    id (Dot.se2id G e)
    (if Tags.hasTag e "external" G then ",style=dotted" else "")
    label role

let suppress_as_text (G : DCR.dcr) suppressed = 
  let labelOf f = Map.find f G.labels |> DCR.label_head
  let roleOf f = Tags.find f "role" G |> String.concat ","
  suppressed 
  |> Seq.map (fun (e, _, _, _) -> e)
  |> Seq.map (fun s -> sprintf "%s (%s)" (labelOf s) (roleOf s))
  |> String.concat "\\r"
  |> (fun s -> s + "\\r")

let emit_idem_box (G : DCR.dcr) id suppressed t = 
  if not <| Seq.isEmpty suppressed then 
    let id' = (G, "suppressed").GetHashCode()
    fprintfn t 
      "%u [shape=ellipse,fontname=\"Roboto Mono\",fontsize=7,label=\"%s\",fontcolor=grey65,color=grey65];" 
      id' (suppress_as_text G suppressed)
    fprintfn t "%u -> %u [dir=none,color=grey65];" id id'


type NetOptions = {
  suppress_idems : SuppressIdems
  suppress_as    : SuppressionMechanism
  traversal      : traversal
}
and SuppressIdems        = All | External | No
and SuppressionMechanism = Tooltip | Node

let default_net_options = {
  suppress_idems = No
  suppress_as    = Tooltip
  traversal      = traverse_all
}

let mk_net_dot options G0 t = 

  let isExternal e = Tags.hasTag e "external" G0
  let exec = trimmed_exec 

  let xor id tooltip = 
    fprintfn t "%u [id=\"%s\",shape=circle,label=\"x\",fontname=\"Arial\",margin=0,width=.22,fixedsize=true,tooltip=\"%s\"];" id (Dot.s2id' id) tooltip
  let entry () = 
    //fprintfn t "_0 [shape=circle,label=\"\",width=0.3];" 
    ()
  let exit id accepting tooltip = 
    let danger = 
      if accepting then
        ""
      else 
        "color=red,"
    fprintfn t "%u [id=\"%s\",shape=doublecircle,%slabel=\"\",width=0.23,tooltip=\"%s\"];" id (Dot.s2id' id) danger tooltip

  fprintfn t "digraph TSRNET {"
  fprintfn t "rankdir=LR"
  fprintfn t "edge [fontname=\"Roboto Mono\",fontsize=9];"
  //fprintfn t "_0 -> %u;" (G0.GetHashCode())
  //entry ()

  let c = ref 0
  
  let f (G : DCR.dcr) x = 
    let x = List.ofSeq x
    let id_src = G.GetHashCode()

    let xs = 
      x 
      |> List.ofSeq
      |> List.map (fun e -> 
        let id_edge = (G, e).GetHashCode()
        let H = exec G e
        let id_tgt = H.GetHashCode()
        e, id_edge, id_tgt, H)

    let active, suppressed = 
      xs 
      |> List.partition (fun (e, _, id_tgt, _) -> 
        id_src <> id_tgt || 
          match options.suppress_idems with
          | All -> false
          | External -> isExternal e
          | No -> true)

    let tooltip =   
      match options.suppress_as with 
      | Tooltip -> suppress_as_text G suppressed
      | Node -> ""

    if List.length active = 0 then 
      exit id_src (DCR.accepting G) tooltip
    else
      xor id_src tooltip

    if options.suppress_as = Node then
      emit_idem_box G id_src suppressed t

    for (e, id_edge, id_tgt, H) in active do
      emit_activity_box G e id_edge t
      let hxs = DCR.executable H 
      if id_src <> id_tgt then 
        fprintfn t "%u -> %u [id=\"%s\"]; %u -> %u [id=\"%s\"];"
          id_src id_edge (Dot.se2id G e) id_edge id_tgt (Dot.se2id G e)
      else
        fprintfn t "%u -> %u [dir=both];" id_src id_edge

  options.traversal f G0
  fprintfn t "}"


(*
let mk_net_dot G t = 

    let trans = "node [shape=box,fontname=\"Roboto Mono\",fontsize=9];"
  
    fprintfn t "digraph TSRNET {"
    fprintfn t "rankdir=LR"
    fprintfn t "edge [fontname=\"Roboto Mono\",fontsize=9];"
    fprintfn t "%u [shape=point];" <| G.GetHashCode()
    
    let c = ref 0

    let f (G : DCR.dcr) x = 
        let n = G.GetHashCode()
        for e in x do
          let id = (G, e).GetHashCode()
          let label = Map.find e G.labels |> DCR.label_head
          let role = Tags.find e "role" G |> String.concat ","
          fprintfn t 
            "%u [shape=box,fontname=\"Roboto Mono\",fontsize=9,label=\"%s\n(%s)\"];" 
            id label role
          let H = (trimmed_exec G G e)
          for f in DCR.executable H do 
            fprintfn t "%u -> %u;" 
              id <| (H,f).GetHashCode()
        false
    check f G |> ignore
    fprintfn t "}"
    *)


let check_route src dst using avoid G = 
    
  // Find a way to execute the starting event. 
  let G1 = 
    match src with 
    | Some src -> 
      match check_event src G with 
      | None, _ -> error 0027 "Start event '%s' not reachable" src
      | Some trs, _ -> Seq.fold (flip DCR.execute) G trs
      |> DCR.execute src 
    | None -> G

  // Find a path from start event to end event
  match check_event_path dst using avoid G1 with
  | None, _ -> error 0028 "End event '%s' not reachable" dst
  | Some trs, stats -> 
      trs 
      |> (fun t -> 
            match src with 
            | None -> t 
            | Some src' -> src' :: t)
      |> flip Seq.append (Seq.singleton dst)

let mk_ind_tsr_dot G t = 

  fprintfn t "digraph TSR {"
  fprintfn t "rankdir=LR"
  fprintfn t "node [shape=box,fontname=\"Roboto Mono\",fontsize=9];"
  fprintfn t "edge [fontname=\"Roboto Mono\",fontsize=9];"

  let f (G : DCR.dcr) x = 
    let n = G.GetHashCode()
    let label = 
      G.events 
      |> Seq.map (fun e -> 
          e + " "
            + if Set.contains e G.insi then "+" else "%"
            + if Set.contains e G.pend then "!" else " "
            + if Set.contains e G.exec then "." else " "
        )
      |> String.concat "\\r"
      |> (fun s -> s + "\\r")
    fprintfn t "%u [label=\"%s\"];" n label

    let externs, interns = 
      x
      |> List.ofSeq
      |> List.partition (fun e -> 
        Tags.hasTag e "external" G)

    let externs = Seq.groupBy (trimmed_exec G) externs
    let interns = Seq.groupBy (trimmed_exec G) interns

    let iter style groups = 
      for (H, es) in groups do 
        es 
        |> Seq.map (fun e -> Map.find e G.labels |> DCR.label_head)
        |> String.concat ","
        |> fprintfn t "%u -> %u [%slabel=\"%s\"]" n (H.GetHashCode()) style

    iter "" interns
    iter "style=dotted," externs
    false
  check f G |> ignore
  fprintfn t "}"


