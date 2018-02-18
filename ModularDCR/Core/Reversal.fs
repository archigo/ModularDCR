module Reversal

open D
open TS

type event = DCR.event
let (%?) = DCR.(%?)

let trace = ref false

type Effect = { 
  I : Set<event>
  X : Set<event>
  P : Set<event>
}

type Phi 
  = Incl of bool * event
  | Pend of bool * event
  | Hist of event
  | True
  | False
  | Disj of Set<Phi>
  | Conj of Set<Phi>

let set = Set.ofList

let rec size = function  
  | Incl _ -> 1
  | Pend _ -> 1
  | Hist _ -> 1
  | True _ -> 1
  | False _ -> 1
  | Disj(disjuncts) -> 1 + Seq.sumBy size disjuncts 
  | Conj(conjuncts) -> 1 + Seq.sumBy size conjuncts


// Implicit conjunction
let executable (G : DCR.dcr) e = 
  Incl(true,e) 
    :: 
      (G.cond_on %? e
       |> Seq.map (fun cond -> Disj(set [Incl(false,cond); Hist cond]))
       |> List.ofSeq)
    @
      (G.mile_on %? e
       |> Seq.map (fun cond -> Disj(set [Incl(false,cond); Pend(false,cond)]))
       |> List.ofSeq)

let whenever p f x = if p x then f x else x


let rec simplify = function
  | Conj(conjuncts) -> 
      let conjuncts = Set.map simplify conjuncts |> Set.filter ((<>) True)
      if Set.isEmpty conjuncts then
        True
      elif Set.exists ((=) False) conjuncts then
        False
      else 
        Conj(conjuncts)

  | Disj(disjuncts) -> 
      let disjuncts = Set.map simplify disjuncts |> Set.filter ((<>) False)
      if Set.isEmpty disjuncts then 
        False
      elif Set.exists ((=) True) disjuncts then
        True
      else
        Disj(disjuncts)
  | x -> x


let transform ev effect phi = 
  let rec transform' phi = 
    match phi with 

    | Incl(value, e) -> 
        let i = Set.contains e effect.I 
        let x = Set.contains e effect.X
        match value, i, x with 
        | true, true, _ -> True
        | true, _, true -> False
        | false, true, _ -> False
        | false, _, true -> True
        | _, _, _ -> phi

    | Pend(value, e) -> 
        match value, Set.contains e effect.P with
        | true, true -> True
        | true, false -> if e = ev then False else phi
        | false, true -> False
        | false, false -> if e = ev then True else phi

    | Hist e -> if ev = e then True else phi

    | True -> True

    | False -> False
        
    | Disj(disjuncts) -> Disj(Set.map transform' disjuncts)

    | Conj(conjuncts) -> Conj(Set.map transform' conjuncts)

  transform' phi |> simplify


let effect_of (G : DCR.dcr) e = 
  { I = G.incl %? e
    X = Set.difference (G.excl %? e) (G.incl %? e)
    P = G.resp %? e
  }

let rec difference (G : DCR.dcr) phi =
  match phi with
  | Incl(value,e) -> if Set.contains e G.insi = value then True else phi
  | Hist e ->        if Set.contains e G.exec         then True else phi
  | Pend(value,e) -> if Set.contains e G.pend = value then True else phi
  | True -> phi
  | False -> phi
  | Disj (disjuncts) -> Set.map (difference G) disjuncts |> Disj
  | Conj (conjuncts) -> Set.map (difference G) conjuncts |> Conj

let models (G : DCR.dcr) phi = 
  match difference G phi |> simplify with
  | True -> true
  | _ -> false

let rec pp = function 
  | Incl(value, e) -> sprintf "I%c(%s)" (if value then '+' else '-') e
  | Hist e -> "H (" + e + ")"
  | Pend(value, e) -> sprintf "P%c(%s)" (if value then '+' else '-') e
  | True -> "T"
  | False -> "F"
  | Conj (conjuncts) -> conjuncts |> Seq.map pp |> String.concat " & "
  | Disj (disjuncts) -> disjuncts |> Seq.map pp |> String.concat " | "


let search (G : DCR.dcr) (pass : event seq) (avoid: event seq) e = 
  let G = DCR.flatten G
  let pass = Set.ofSeq pass
  let avoid = Set.ofSeq avoid
  DCR.verify_events G pass
  DCR.verify_events G avoid
  let effects = 
    G.events 
    |> Seq.filter (fun e -> not (Set.contains e avoid))
    |> Seq.map (fun e -> (e, effect_of G e)) |> List.ofSeq
    |> Seq.filter (fun (e, _) -> not (Set.contains e (G.cond_on %? e)))
    |> List.ofSeq
    |> List.sortBy (fun (_, effect) -> (Set.count effect.X))
        // Prefer not excluding things, if no other considerations apply.

  let moves conj = 
    if !trace then
      printfn "Investigating clause: \n\t%s" (pp conj)
    effects 
    |> List.map (fun (e, effect) -> e, transform e effect conj)
    |> List.filter (fun (e, conj') -> (conj' <> False && conj' <> conj))
    |> List.sortBy (snd >> difference G >> simplify >> size) 
          // Try those steps that remove the most unsatisfied constraints first. 
    |> fun x -> 
          if !trace then
            printfn "\tMoves: %A" (Seq.map fst x |> Set.ofSeq)
          x

  let rec move clause (e, conj') = 
    if !trace then
      printfn "MOVE: %s" e
    // Must preserve CNF. 
    match conj' with 
    | True -> move clause (e, Conj(Set.empty)) 
    | Conj(conjuncts) -> 
        Set.union conjuncts (set <| executable G e) |> Conj
    | _ -> failwithf "CNF violation: %A." conj'

  let state0 = 
    let execs = executable G e |> set 
    let pass = Set.map (fun e -> Hist e) pass
    Conj (Set.union execs pass)
    |> D.dbg

  let ts = {
    actions = moves
    move = move
    state0 = state0 
    gen = List.toSeq
  }
  let P conj _ = 
    //printf "Checking clause \n\t%s ?" (pp_conj conj)
    let x = models G conj
    //printfn " %A." x
    x
  let res, stats = TS.dfs ts P
  let res' = res |> Option.map (List.map fst >> List.rev)

  (res', stats)

  

  
(*
 open Reversal;
 let G = Loader.loadfile "../../../Test/XML/dreyer.xml";
 search G [] [] "Receive End Report";
 search G ["First Payment"] [] "Receive End Report"
 *)
