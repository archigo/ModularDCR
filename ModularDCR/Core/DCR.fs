module DCR

  open D

  (* DCR graph data structure and dynamics. 
   *
   * This implementation is based on [1], with extensions for time from [2], 
   * and various other extensions:
   *
   * [1] Describes DCR graphs with sub-processes and their composition. 
   * [2] Describes the addition of time. 
   *
   * This implementation differs in important respects:
   * - We have notions of "input" and "output" labels;
   * - Sub-processes may input values; instantiation of sub-process relations
   *   maybe conditional upon the values received.  
   * - Sub-process instantiation treats local events of the ambient process as
   *   binding in the instantiated sub-process.
   *)

  let infinity = Microsoft.FSharp.Core.int.MaxValue


  type event = string

  (* Highlights. *)

  module Highlights = 
    type Location = int * int * string
    type Markup = 
    | Event of name: string * role: string 
    | Relation of kind: string * src: string * tgt: string 
    | Role of name: string 
    | No 
    type Highlights = (Markup * Location) list

  (* Labels of events in [1] are unstructured; here, we distinguish between
   * input, output, and ordinary labels. *)
  type label =
    | Input of string 
    | Output of string * Data.expr 
    | Computation of string * Data.expr
    | Neutral of string 

  (* With input and output events, "executing" in a DCR graph is no longer
   * just executing a single event; we have also received values or
   * synchronization. 
   *)
  type action = 
       | Event of event                       
       | Recv of event * Data.value list // ... from environment. Matches Input label.
       | Sync of event * event

  (* DCR graph data structure. *)
  type dcr = {
      (* [1, Def 4.1] *)
      events  : Set<event>
      labels  : Map<event, label>

      (* Relations. *)
      incl    : Map<event, Set<event>> // _ -->+ _ 
      excl    : Map<event, Set<event>> // _ -->% _
      resp    : Map<event, Set<event>> // _ *--> _
      cond_on : Map<event, Set<event>> // _ *<-- _
      mile_on : Map<event, Set<event>> // _ <>-- _

      (* Marking. *)
      exec : Set<event>
      insi : Set<event>   (* 'incl' is the relation; say 'inside' or 'outside'. *)
      pend : Set<event>
      
      (* Interface & sub-processes. [1, Def. 4.12] *)
      local : Set<event>
      defs : Map<event, Set<dcr>>

      (* Grouping. *)
      groups : Grouping.partition<event, event>

      (* Guarded (conditional) definitions. Variation on [3, Def. 4.33]. 
       * Invariant: The embedded DCR is contains only relations, no local or
       * defs.  The parser guarantees as much. *) 
      conds : List<Data.expr * dcr>

      (* Tags (extra information associated with events). *)
      tags    : Map<event, Map<string, Set<string>>>

      (* Store *)
      store : Map<string, Data.value>

      (* Highlights *)
      highlights: Highlights.Highlights
      text: string

      (* Time. [2, Def. 3.2] *)
      t_ex : Map<event, int>      (* Invariant: Defined everywhere on Ex. *)
      t_re : Map<event, int>      
      t_c  : Map<event * event, int>
      t_r  : Map<event * event, int>

  } 
  with 
      member G.iface = 
        Set.difference G.events G.local
      member G.rec_iface = 
        let rec loop local G = 
            let local' = Set.union local G.local
            let iface = Set.difference G.events local'
            Map.fold (fun es _ Hs ->
                          Set.fold (fun es' H -> Set.union es' (loop local' H)) es Hs
                          |> Set.union es)
                     iface
                     G.defs
        loop Set.empty G
      (* [2, Def. 3.2] *)
      member dcr.maxc = Map.fold (fun m _ i -> max i m) 0 dcr.t_c
      member dcr.minr = Set.intersect dcr.pend dcr.insi
                        |> Seq.map (fun e -> Map.tryFind e dcr.t_re)
                        |> Seq.map (function None -> infinity | Some n -> n)
                        |> Seq.fold min infinity

            
  let (%?) ss e = match Map.tryFind e ss with
                        Some s -> s
                      | None -> Set.empty


  let labelOf G e = D.safeFind e G.labels
  let eventsOf' G l = 
    G.labels 
    |> Map.toSeq
    |> Seq.filter (snd >> (=) l)
    |> Seq.map fst
    |> Set.ofSeq
  let eventsOf G str = eventsOf' G (Neutral str)

  // Filter G by removing events not satisfying P.
  let rec filter P G = 
      let fmap x = Map.filter (fun e _ -> P e) x
      // Bullshit F# type-inference won't generalise unless I eta-expand.
      let frel r = fmap r |> Map.map (fun _ es -> Set.filter P es)
      { 
          events  = Set.filter P G.events
          labels  = G.labels |> fmap
          incl    = G.incl    |> frel
          excl    = G.excl    |> frel
          resp    = G.resp    |> frel 
          cond_on = G.cond_on |> frel
          mile_on = G.mile_on |> frel
   
          exec = Set.filter P G.exec
          insi = Set.filter P G.insi
          pend = Set.filter P G.pend
   
          (* Interfaces & sub-processes. *)
          defs  = G.defs 
                  |> fmap
                  |> Map.map (fun _ dcrs -> Set.map (filter P) dcrs)
          local = Set.filter P G.local

          (* Grouping. *)
          groups = Grouping.map (Set.filter P) G.groups 

          (* Guards. *)
          conds = G.conds |> List.map (fun (g,G) -> (g, filter P G))
          (* TODO: Unnecessarily re-computes sigma'/transform each step. *)

          tags = G.tags |> fmap 
          store = G.store // TODO: ?

          highlights = G.highlights
          text = G.text

          (* Time. *)
          t_ex = G.t_ex |> fmap
          t_re = G.t_re |> fmap
          t_c  = G.t_c |> Map.filter (fun (e,f) _ -> P e && P f)
          t_r  = G.t_r |> Map.filter (fun (e,f) _ -> P e && P f)
      }

  (* Structure-preserving dcr graph event-renaming.
   * Assumes sigma bijection. *)
  let rec rename sigma G =
      let transform sigma = Map.toList >> List.map sigma >> Map.ofList
      let sigma' = transform (fun (e, es) -> sigma e, Set.map sigma es)
      { 
          events  = Set.map sigma G.events
          labels  = G.labels |> transform (fun (e, l) -> (sigma e, l))

          (* Relations. *)
          incl    = G.incl    |> sigma'
          excl    = G.excl    |> sigma'
          resp    = G.resp    |> sigma' 
          cond_on = G.cond_on |> sigma'
          mile_on = G.mile_on |> sigma'

          (* Marking. *)
          exec = Set.map sigma G.exec
          insi = Set.map sigma G.insi
          pend = Set.map sigma G.pend

          (* Interfaces & sub-processes. *)
          defs  = G.defs |> transform (fun (e, Gs) -> sigma e, Set.map (rename sigma) Gs) 
          local = Set.map sigma G.local

          (* Grouping. *)
          groups = Grouping.map (Set.map sigma) G.groups 

          (* Guards. *)
          conds = G.conds |> List.map (fun (g,G) -> (g, rename sigma G))

          tags = G.tags |> Map.toSeq |> Seq.map (fun (e,x) -> sigma e, x) |> Map.ofSeq 

          (* Store *)
          store = G.store // TODO: renaming not considered. *)

          highlights = G.highlights
          text = G.text

          (* Time. *)
          t_ex = transform (fun (e, i) -> sigma e, i) G.t_ex
          t_re = transform (fun (e, i) -> sigma e, i) G.t_re
          t_c  = transform (fun ((e, f), i) -> (sigma e, sigma f), i) G.t_c
          t_r  = transform (fun ((e, f), i) -> (sigma e, sigma f), i) G.t_r
      }

  let empty = {
          events=Set.empty
          labels=Map.empty

          (* Relations. *)
          incl=Map.empty
          excl=Map.empty
          resp=Map.empty
          cond_on=Map.empty
          mile_on=Map.empty

          (* Marking. *)
          exec=Set.empty
          insi=Set.empty
          pend=Set.empty

          (* Interfaces & sub-processes. *)
          defs=Map.empty
          local=Set.empty

          (* Groupings. *)
          groups=Grouping.empty 

          (* Guards. *)
          conds=[]

          (* Tags. *)
          tags=Map.empty

          (* Store. *)
          store=Map.empty

          highlights = []
          text = ""

          (* Time. *)
          t_ex = Map.empty
          t_re = Map.empty
          t_c = Map.empty
          t_r = Map.empty
  }


  (* DCR graphs g,h are compatible if they 
   * can be composed without renaming any local events. *)
  let compatible c d = 
      Set.intersect c.events d.local |> Set.isEmpty &&
      Set.intersect d.events c.local |> Set.isEmpty 

  let has_subprocesses G = G.defs |> Map.exists (fun k -> Set.isEmpty >> not)

  (* True if every event of G has unique label. This approximates label determinism. *)
  let is_definite G = 
      G.labels |> Map.toSeq |> Seq.countBy snd |> Seq.forall (snd >> (=) 1) 
      
  (* CAUTION! 
   *
   * We make fresh names using this counter.  This makes this DCR-implementation
   * inherently thread-unsafe. To make matters worse, the counter is temporarily
   * messed with in the local independence check below.
   *)
  let mutable k = 0


  (* Rename local events of H to guarantee that no local event of H is caught by an event of G. *)
  let separate' (G : dcr) (H : dcr) = 
      let avoid = Set.unionMany [ G.rec_iface; G.events; H.rec_iface; H.events ]
      let conflicts = (Set.union G.rec_iface G.events) |> Set.intersect <| H.local
      let sigma e = 
          if Set.contains e conflicts then
              let rec loop k =
                  let proposed = sprintf "%s$%d" e k
                  if Set.contains proposed avoid then loop (k+1)
                  else proposed
              loop 1
          else
              e

      let H' = rename sigma H

      H', sigma

  (* Rename local events of G,H to ensure that no local event of one is caught by
   * some event of the other. *)
  let separate G0 H0 = 
      let H, sigma_H = separate' G0 H0
      let G, sigma_G = separate' H G0
      G, H, sigma_G, sigma_H


  (* For error-messages. *)
  let ppLabel = function 
      | Output (h, expr) -> sprintf "%s<%s>" h <| Data.ppexpr expr
      | Input h -> sprintf "%s?" h 
      | Neutral h -> h
      | Computation (h, expr) -> sprintf "%s[[=%s]]" h <| Data.ppexpr expr


  (* Unsafe component-wise union. Incompatible dcr graphs will
   * be union'ed indiscriminately. *)
  let unchecked_union allow_label_mismatch G H = 
      let label_union = 
          let combine e l l' = 
              match l <> l', allow_label_mismatch with
              | false, _ -> l
              | true, true when l  = Neutral e -> l'
              | true, true when l' = Neutral e -> l
              | _ -> error 0001 "Event '%s' labelled both by '%s' and '%s'"
                                 e (ppLabel l) (ppLabel l')
          Map.merge combine

      let tag_union = Map.merge (fun _ -> Map.union)
      let time_union_1 = 
          let combine e k k' = 
            if k = k' then k else error 0041 "Incompatible time-histories %d and %d for event '%s'" k k' e
          Map.merge combine

      let time_union_2 rel = 
          let combine (e, f) k k' =
            if k = k' then k else error 0042 "Incompatible time-constraints %d and %d for events '%s' and '%s', relation '%s'" k k' e f rel
          Map.merge combine

      {
          events  = G.events  |> Set.union <| H.events
          labels  = G.labels  |> label_union <| H.labels

          mile_on = G.mile_on |> Map.union <| H.mile_on
          cond_on = G.cond_on |> Map.union <| H.cond_on
          incl    = G.incl |> Map.union <| H.incl
          excl    = G.excl |> Map.union <| H.excl
          resp    = G.resp |> Map.union <| H.resp

          exec    = G.exec |> Set.union <| H.exec
          insi    = G.insi |> Set.union <| H.insi
          pend    = G.pend |> Set.union <| H.pend


          local   = G.local |> Set.union <| H.local
          defs = G.defs |> Map.union <| H.defs

          groups = Grouping.merge G.groups H.groups

          conds = G.conds @ H.conds

          tags = G.tags |> tag_union <| H.tags
          store = 
            let merge = 
              Map.merge (fun e l l' -> 
                if l = l' then 
                  l 
                else 
                  error 0038 "Incompatible for global key '%s': '%s' vs '%s'" e (Data.ppval l) (Data.ppval l'))
            G.store |> merge <| H.store

          highlights = 
            if G.highlights = [] && H.highlights = [] then 
              [] 
            else 
              D.error 0039 "Merging of highlight-augmented graphs not implented."

          text = G.text + H.text

          (* Not in any paper. We insist that both graphs agree /exactly/ on time
           * limits in both relation and marking. *)
          t_re = G.t_re |> time_union_1 <| H.t_re
          t_ex = G.t_ex |> time_union_1 <| H.t_ex
          t_c  = G.t_c  |> time_union_2 "condition" <| H.t_c
          t_r  = G.t_r  |> time_union_2 "response" <| H.t_r

      }


  (* Arbitrary marking merge. *)
  let merge' G0 H0 =
      let G, H, sigma_G, sigma_H = separate G0 H0
      unchecked_union false G H, sigma_G, sigma_H  

  let merge G = merge' G >> fun (G, _, _) -> G

  // Merge G H, but state of G takes precedence over state of H, 
  // and groups of G capture events of H. 
  let unchecked_subsuming_union G H = 
      let shared = Set.intersect G.events H.events
      let gnames = Grouping.names G.groups |> Set.ofSeq
      let clear_additions x y z = 
          Set.difference x 
            <| Set.union 
                 (Set.intersect shared (Set.difference z y))
                 gnames
      let I = unchecked_union true G H
      { I 
        with events = Set.difference I.events gnames
             insi = clear_additions I.insi G.insi H.insi
             exec = clear_additions I.exec G.exec H.exec
             pend = clear_additions I.pend G.pend H.pend
      }



  (* Merge G H, renaming local events of H as necessary and putting events of H
   * not in the interface of G in a group.  This operation is capturing: local
   * events of G will capture global events of H.  This operation is only
   * meaningful for folding defs onto outer graph. *)
  let subsume e G H = 
      let H, sigma_H = separate' G H
      { unchecked_subsuming_union G H  
        with 
          groups = 
            let Hlocal = Set.difference H.events (Set.union G.events (Grouping.names G.groups |> Set.ofSeq))
            if Hlocal <> Set.empty then 
              let h = None, Hlocal, Grouping.map (Set.intersect Hlocal) H.groups
              Grouping.add G.groups e <| Grouping.Partition (Set.singleton h)
            else
              G.groups
      }, sigma_H

  (* Determine if a marking merge is meaningful and 
   * locally reductive. 
   * 
   * TODO: Does this even make sense in the presence of 
   * subprocess definitions?
   *)
  let merge_verify_safe (G : dcr) (H : dcr) = 
      let causes e = Map.fold (fun cs f fs -> if Set.contains e fs then f :: cs else cs) [] 
      let shared = G.iface |> Set.intersect <| H.iface 

      (* TODO: Add static check that subprocess definitions, when spawning,
       * will not violate safety. *)

      shared
      |> Set.filter (fun e -> not (causes e G.excl = causes e H.excl &&
                                   causes e G.incl = causes e H.incl))
      |> Set.iter (fun e -> 
          error 0003 "Shared event '%s' is the target of a non-shared inclusion or exclusion." e)

      shared
      |> Set.filter (fun e -> not (Set.contains e G.insi = Set.contains e H.insi))
      |> Set.iter (fun e -> 
          error 0004 "Shared event '%s' is included in one marking but excluded in the other." e)

      (* Observe that we do not require "pending" or "executed" status of 
       * shared events to be the same. *)

  let merge_would_be_safe G H = 
    try
      merge_verify_safe G H
      true
    with 
      _ -> false

  let merge_safe G H = merge_verify_safe G H
                       merge G H

  let is_event G e = Set.contains e G.events

  let verify_event G e = 
      (is_event G e || error 0005 "Unknown event '%s'" e) |> ignore

  let verify_events G = Seq.iter (verify_event G) 

  (* Determine if marking G is accepting. Variant of [1, Def 4.5] *)
  let accepting G = Set.intersect G.pend G.insi |> Set.isEmpty

  let flatten (G : dcr) = 
    let names = Grouping.names G.groups |> Set.ofSeq
    let pack = Seq.fold (fun m (e, es) -> Map.insertMany e es m) Map.empty
    let fix rel = 
      rel 
      |> Map.toSeq 
      |> Seq.collect (fun (e,es) -> 
          let srcs = 
            Grouping.descendants G.groups e
          let tgts = 
            Grouping.bridge G.groups e ((%?) rel)
            |> Set.union (Set.difference es names)
          Seq.map (fun src -> src, tgts) srcs)
      |> pack

    let fix' rel = 
      rel 
      |> Map.toSeq
      |> Seq.collect (fun ((e, f), k) -> 
          let es = Grouping.descendants G.groups e
          let fs = Grouping.descendants G.groups f
          seq { 
            for x in es do
              for y in fs do 
                yield (x, y), k
          })
      // FIXME: May exhibit bug fixed by use of pack in fix
      |> Map.ofSeq
      
    { G with
        incl = fix G.incl
        excl = fix G.excl
        resp = fix G.resp
        cond_on = fix G.cond_on
        mile_on = fix G.mile_on
        t_c = fix' G.t_c
        t_r = fix' G.t_r
    }


  (* Produce the set of events immediately executable in marking G. 
   * [1, Def 4.3] and [2, Def 3.3]. 
   *) 
  let executable G0 =
      let G = flatten G0

      G.insi 
          (* From the set of included events ... *)
          |> Set.filter (fun e -> 
          (* ... remove those with included, non-executed conditions ... *)
              G.cond_on %? e
              |> Set.intersect G.insi
              |> Set.isSuperset G.exec
          (* ... and those with included, pending milestones ... *) 
              && G.mile_on %? e
              |> Set.intersect G.insi
              |> Set.intersect G.pend
              |> Set.isEmpty
          (* ... and those with conditions that have yet to timeout. *)
              && G.cond_on %? e
              |> Set.intersect G.insi 
              |> Set.intersect G.exec
              |> Set.forall 
                  (fun e' -> match Map.tryFind(e',e) G.t_c with
                             | Some k -> k <= Map.find e' G.t_ex
                             | None   -> (* k = 0 *) true)
            )

      (* There's some duplication of computation in the above, in the interest of preserving
       * the clearest possible connection with research papers. *)


  let is_executable G e = Set.contains e (executable G) 


  let rec subst symtab G = 
      { G with 
          labels = 
              G.labels 
              |> Map.map (fun e -> function 
                                   | Output (h, expr) -> Output (h, Data.subst symtab expr)
                                   | x -> x)
          defs = 
              G.defs 
              |> Map.map (fun e Gs -> Gs |> Set.map (subst symtab))
          conds = G.conds |> List.map (fun (g, G') -> Data.subst symtab g, G') 
                  // Correct because by convention G' contains no defs. 

      }

  let rec instantiate symtab G = 
      let G0 = subst symtab G
      (* Force evaluation at the top-level. *)
      let G1 = { G0 with 
                  labels = G.labels 
                           |> Map.map (fun e -> function
                                                | Output (h, expr) -> Output (h, Data.subst symtab expr)
                                                | l -> l)
                  conds = [] 
                  tags = 
                    G0.tags
                    |> Map.map (fun e ts -> 
                        ts |> Map.map (fun t strs -> 
                          strs |> Set.map (fun (str : string) -> 
                            if str.StartsWith("$") && not (str.StartsWith("$$")) then
                              let var = str.Substring(1)
                              Data.eval symtab (Data.Var var) |> Data.stringv
                            else
                              str)))
               } 
      G0.conds 
      |> Seq.filter (fst >> Data.eval symtab >> Data.boolv)
      |> Seq.map snd
      |> Seq.fold (fun Ga Gx -> { Gx with labels = G1.labels } |> unchecked_union false Ga)
                  G1
                  // Terrible hack to ignore instantiated labels of
                  // G1 mismatching un-instantiated labels of G'. 
                  // This is an argument that representing conditionals
                  // as entire DCR graphs when we only care about
                  // relations is a bad idea. 
     

  (* Assuming event e is executable in marking G, produce "next" marking. 
   * [1, Def 4.16] *)
  let rec execute_with_instantiation e arguments G0 = 

      (* Check arity. *)
      let parameters = match Map.find e G0.labels with
                       | Input _  -> [e]
                       | _ -> [] 
                       
      if List.length arguments <> List.length parameters then 
          error 0006 "Expected %d arguments, got %d." 
                      (List.length parameters) (List.length arguments) 

      let symtab = Map.ofSeq (Seq.zip parameters arguments)

      (* Spawn new subgraphs. [1, Def 4.16(1)], however, the present
       * implementation deviates in allowing local names of G to capture shared
       * names of its definitions. E.g., "/a{ a *--> /b}" cannot evolve to a DCR
       * graph containing more than one event labelled "a". 
       *)
      let G, sigma = G0.defs %? e 
                     |> Set.fold (fun (G1, s) G2 -> let G2' = instantiate symtab G2 
                                                    let (H, s') = subsume e G1 G2'
                                                    (H, s >> s'))
                                 (G0, fun x -> x)

      let G = 
        G.conds 
        |> Seq.filter (fst >> Data.eval symtab >> Data.boolv)
        |> Seq.map snd
        |> Seq.fold (unchecked_union false) G


      let H = flatten G
      {     
        (* Execute the transition. *)
        G with
          (* [1, Def 4.3] *)
          exec = Set.add e H.exec
          insi = Set.difference H.insi (H.excl %? e)
                 |> Set.union (H.incl %? e)
                 (* If "A -->% B A -->+ B" then executing A
                    will include B. *)
          pend  = Set.remove e H.pend |> Set.union (H.resp %? e)
          store = symtab |> Map.fold (fun m k v -> Map.add ("$" + k) v m) H.store

          (* Time. [3, Def. 3.3] *)
          t_ex = Map.add e 0 H.t_ex
          t_re = 
              H.resp %? e 
                 |> Set.fold (fun r e' -> 
                      let d' = 
                        match Map.tryFind (e,e') H.t_r with
                        | Some k -> k
                        | None   -> infinity
                      let d = 
                        match Map.tryFind e' H.t_re with 
                        | Some x -> max x d'
                        | None -> d'
                      Map.add e' d r)
                      (Map.remove e H.t_re)
     
      }

  let execute e G = 
      match Map.find e G.labels with 
      | Input _ -> error 0007 "Event '%s' requires %d input arguments." e 1
      | _ -> execute_with_instantiation e [] G


  (* Advancing time. [3, Def. 3.3]. *)
  let advancable n (G : dcr) = G.minr >= n
  let advance n (G : dcr) = 
      if not (advancable n G) then
          error 0040 "Advancing time by %d units would miss a deadline." n
      else
          let maxc = G.maxc
          { 
              G with
                  t_ex = G.t_ex |> Map.map (fun _ k -> min (k+n) maxc)
                  t_re = G.t_re |> Map.map (fun _ k -> if k = infinity then infinity else max (k-n) 0)
          }

  (* Synchronize input event e with output event f. *) 
  let execute_communication e f G = 
      let parameters = match Map.find e G.labels with
                       | Input h -> [e]
                       | _ -> error 0008 "Event '%s' does not take inputs." e
      let arguments =  match Map.find f G.labels with
                       | Output (h, expr) -> 
                           failwith "Not implemented."
                          (*
                            List.map (function 
                                      | k, Data.Const x -> x
                                      | k, v -> error 0010 "Cannot evaluate '%s' in output event '%s'" (Data.ppexpr v) f)
                                     args
                          *)
                       | _ -> error 0009 "Event '%s' does not produce output." f

      let m = List.length parameters 
      let n = List.length arguments 
      if m <> n then
          error 0011 "Arity mismatch: %s expects %d inputs, but %s produces %d outputs."
                      e m f n

      (* To synchronize, e and f must be locally independent, i.e., 
       * have the little concurrency diamond. This requires us to 
       * compute equality of DCR graphs, which is difficult because 
       * of local names. As it happens, mechanics of local name 
       * generation does respect commutativity of the necessary diagrams. *)
      let G1 = G |> execute_with_instantiation e arguments
      let G2 = G |> execute f
      if not (is_executable G1 f && is_executable G2 e) then
          error 0012 "Events '%s' and '%s' are not locally independent." e f
      let G3  = G1 |> execute f
      let G3' = G2 |> execute_with_instantiation e arguments
      if not (G3 = G3') then
          error 0013 "Events '%s' and '%s' are not locally independent." e f
      G3

  let execute_action action G = 
      let check = Seq.iter (fun e -> 
          if not <| is_executable G e then
              error 0014 "Event '%s' is not executable." e)
      
      match action with 
      | Event e -> check [e]; execute_with_instantiation e [] G
      | Recv (e, args) -> 
          check [e]
          match Map.find e G.labels with
          | Input h ->
              let m = 1
              let n = List.length args
              if m <> n then 
                  error 0015 "Arity mismatch: %s expects %d inputs, got %d."
                              e m n
              execute_with_instantiation e args G
          | l -> error 0016 "%s is labelled %s, which is not an input."
                             e (ppLabel l)
      | Sync (e, f) -> 
          check [e; f]
          execute_communication e f G

  let eventOf G label = 
    G.labels 
      |> Map.pick (fun ev lab -> if lab = Neutral label then Some ev else None)


  let project G evts = 
    let G = filter (flip Set.contains evts) G 
    let G = { G with groups = G.groups |> Grouping.removeEmpty } 
    let names = Grouping.names G.groups |> Set.ofSeq
    let remains = Set.union names evts
    let clr = 
      Map.map (fun _ F -> Set.intersect remains F)
      >> Map.filter (fun e F -> 
          Set.contains e remains && (not <| Set.isEmpty F))
    { G with
        incl = clr G.incl
        excl = clr G.excl
        resp = clr G.excl
        cond_on = clr G.cond_on
        mile_on = clr G.mile_on 
        groups = G.groups |> Grouping.removeEmpty }

  let label_head = function 
      | Neutral l -> l
      | Output (l, _) -> l
      | Input (l) -> l
      | Computation (l, _) -> l

  let show_label G e = labelOf G e |> label_head

  let show_events = Set.toList >> String.concat " " >> printf "%s"

  let show_labelled_event G e = 
      let ann l = if e=l then "" else sprintf "[%s] " l
      e + (ppLabel <| Map.find e G.labels)

  let diff G H =
    seq { 
      let linewise strs = 
        let strs' = Seq.map (sprintf "%A") strs 
        "\n  " + String.concat "\n  " strs' + "\n"
      let diffSets title X Y = 
        seq { 
          let xnoty = Set.difference X Y
          if xnoty <> Set.empty then 
            yield (sprintf "%s in left only: %s" title (linewise xnoty))
          let ynotx = Set.difference Y X 
          if ynotx <> Set.empty then 
            yield (sprintf "%s in right only: %s" title (linewise ynotx))
        }
      let diffMaps title (X : Map<'T,'U>) (Y : Map<'T,'U>) = 
        diffSets title (X |> Map.toSeq |> Set.ofSeq) (Y |> Map.toSeq |> Set.ofSeq)
      yield! diffSets "events"  G.events  H.events
      yield! diffMaps "labels"  G.labels  H.labels
      yield! diffMaps "incl"    G.incl    H.incl
      yield! diffMaps "excl"    G.excl    H.excl
      yield! diffMaps "resp"    G.resp    H.resp
      yield! diffMaps "cond_on" G.cond_on H.cond_on
      yield! diffMaps "mile_on" G.mile_on H.mile_on
      yield! diffSets "exec"    G.exec    H.exec
      yield! diffSets "insi"    G.insi    H.insi
      yield! diffSets "pend"    G.pend    H.pend
      yield! diffSets "local"   G.local   H.local
      if G.defs <> H.defs then
        yield "defs differ"
      if G.groups <> H.groups then
        yield "groups differ"
      if G.conds <> H.conds then
        yield "conds differ"
      let cmptags e X Y =
        if Set.contains e X.events && Set.contains e Y.events then 
          let xtags = Map.find e X.tags
          let ytags = Map.find e Y.tags
          diffMaps ("tags for " + e) xtags ytags
        else
          Seq.empty
      for e in G.events do yield! cmptags e G H
      for e in H.events do yield! cmptags e H G
      yield! diffMaps ("value for store key") G.store H.store
    }


  let show_labelled_events G = 
      Set.toList >> List.map (show_labelled_event G)
      >> String.concat " " >> printfn "%s"

  let show_dcr G = 
      let rec loop s G = 
          let pr r = Map.iter (fun x ys -> printf "%s%s %s " s x r 
                                           show_events ys 
                                           printfn "")
          printf "%s" s
          G.events |> show_labelled_events G
          G.incl |> pr "-->+"
          G.excl |> pr "-->%"
          G.resp |> pr "*-->"
          G.cond_on |> pr "*<--"
          G.mile_on |> pr "<>--"
          G.defs |> Map.iter (fun e ms -> printfn "%s%s" s e
                                          ms |> Set.iter (fun G -> printfn "%s{" s
                                                                   loop (s + "    ") G
                                                                   printfn "%s}" s))
      loop "" G

  let show_marking G = 
      printf "Executed: "; G.exec |> show_events; printfn ""
      printf "Included: "; G.insi |> show_events; printfn ""
      printf "Pending:  "; G.pend |> show_events
      if Set.isEmpty G.pend then printfn " (accepting)" else printfn ""
      
  let show_state G = 
      let xs = executable G
      G.events
      |> Set.toList
      |> List.map (fun e -> if Set.contains e G.insi then "" else "%"
                            + if Set.contains e xs then "*" else ""
                            + if Set.contains e G.local then "/" else ""
                            + if Set.contains e G.pend then "!" else ""
                            + e) 
      |> String.concat " " |> printfn "%s"
      printfn "%s" <| if accepting G then " (accepting)" else ""
      

      
  (* Bibliography. 
   *
   * [1] Søren Debois, Thomas Hildebrandt and Tijs Slaats:
   *     "Hierarchical Declarative Modelling with Refinement and Sub-processes"
   *     International Conference on Business Process Management (BPM 2014), September 2014, Haifa, Israel.
   *
   * [2] Thomas Hildebrandt, Raghava Rao Mukkamala, Tijs Slaats and Francesco Zanitti:
   *     "Contracts for cross-organizational workflows as timed Dynamic Condition Response Graphs",
   *     The Journal of Logic and Algebraic Programming, vol. 82:5-7, pp. 164-185, Elsevier, 2013. 
   *
   * [3] Raghava Rao Mukkamala:
   *     "A Formal Model For Declarative Workflows"
   *     PhD Thesis, IT University of Copenhagen, 2012. 
   *)


