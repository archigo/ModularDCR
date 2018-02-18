module Concurrency

open D

let (%?) = DCR.(%?)


// BPM '15 notion of independence
let independent e f (G : DCR.dcr) = 
  let dj x y = Set.intersect x y = Set.empty
  (e <> f) &&
  // 4.2(1)
  (dj <| G.incl %? e <| G.excl %? f) && 
  (dj <| G.excl %? e <| G.incl %? f) &&
  // 4.2(2)
  (if Set.contains f (G.resp %? e) then Set.contains f (G.resp %? f) else true) && 
  (if Set.contains e (G.resp %? f) then Set.contains e (G.resp %? e) else true) && 
  // 4.4(1)
  (Set.contains e (G.cond_on %? f) |> not) &&
  (Set.contains f (G.cond_on %? e) |> not) &&
  // 4.4(2)
  (Set.contains e (G.excl %? f) |> not) &&
  (Set.contains f (G.excl %? e) |> not) &&
  (Set.contains e (G.incl %? f) |> not) &&
  (Set.contains f (G.incl %? e) |> not) &&
  // 4.4(3) 
  (dj <| Set.union (G.incl %? e) (G.excl %? e) <| G.cond_on %? f) &&
  (dj <| Set.union (G.incl %? f) (G.excl %? f) <| G.cond_on %? e)


// Thomas Hildebrandt, Raghava Rao Mukkamala & Tijs Slaats, 
// "Safe Distribution of Declarative Processes", SEFM 2011

let preimage E R = 
  Map.fold 
    (fun S e F ->
      if Set.intersect E F <> Set.empty then 
        Set.add e S 
      else 
        S)
    Set.empty
    R

let preimage1 e = preimage <| Set.singleton e 


// Definition 3, part (i). Notes: 
//   1. Since we use it for visualisation, its convenient to retain the labels
//   of external events.
//   2. We made the obvious addition of milestones.
//   3. We leave out e itself.
let modifiers (G : DCR.dcr) e = 
  [ G.cond_on %? e
    preimage1 e G.resp
    preimage1 e G.incl
    preimage1 e G.excl
    preimage (G.cond_on %? e) G.excl 
    preimage (G.cond_on %? e) G.incl 
    // Additions for milestones
    preimage (G.mile_on %? e) G.excl 
    preimage (G.mile_on %? e) G.incl 
  ] 
  |> Seq.collect id
  |> Seq.collect (Grouping.descendants G.groups)
  |> Set.ofSeq
    
// Definition 3.
let project (G : DCR.dcr) E =      // aka. E_\delta
  let E = 
    E |> Seq.collect (Grouping.ancestors G.groups)
      |> Set.ofSeq
  let modifiers = 
    Set.fold 
      (fun S e -> Set.union S (modifiers G e))
      Set.empty 
      E
    |> flip Set.difference E
  let Ex = Set.union modifiers E   // part (i), aka. E_{G|\delta} 
  let conds_miles = 
    Set.fold 
      (fun S e -> Set.unionMany [S; G.cond_on %? e; G.mile_on %? e])
      Set.empty
      E
  let conds_miles_self = Set.union E conds_miles
  let G' =                         
    { DCR.project G Ex
      with 
        // part (iii) + milestone variant
        // exec = ... done by DCR.project
        pend = Set.intersect G.pend E
        insi = 
         Set.union 
            (Set.intersect G.insi conds_miles_self)
            (Set.difference modifiers conds_miles_self)
         |> flip Set.difference (Grouping.names G.groups |> Set.ofSeq)
        // part (iv)
        cond_on = G.cond_on |> Map.filter (fun e _ -> Set.contains e E)
        // part (iv), milestone variant 
        mile_on = G.mile_on |> Map.filter (fun e _ -> Set.contains e E)
        resp = 
          G.resp 
          |> Map.map (fun _ F -> Set.intersect F E)
          |> Map.filter (fun _ F -> not (Set.isEmpty F))
        incl = 
          G.incl 
          |> Map.map (fun _ F -> Set.intersect F conds_miles_self)
          |> Map.filter (fun _ F -> not (Set.isEmpty F))
        excl = 
          G.excl 
          |> Map.map (fun _ F -> Set.intersect F conds_miles_self)
          |> Map.filter (fun _ F -> not (Set.isEmpty F))
    }
  modifiers
  |> flip Set.difference (Grouping.names G.groups |> Set.ofSeq)
  |> Seq.fold (fun H e -> Tags.add e "external" "" H) G'

let rec fix f S = 
  let S' = f S 
  if S' = S then
    S
  else 
    fix f S'

let slice G E = 
  let E' = 
    fix (fun S -> Set.fold (fun S' e -> modifiers G e |> Set.union S') S S) E
  DCR.project G E'


// Asymmetric conflicts. e conflicts with f iff
// e touches the state of f.
// NB! This is likely not what you want. 
let conflicts e f (G : DCR.dcr) = 
  Set.contains f
  <| Set.unionMany [ 
    G.incl %? e
    G.excl %? e
    G.resp %? e
    G.cond_on %? e
    G.mile_on %? e
  ]

open Bitvector.Operators

// Given a set of events, find the maximal subset of mutually independent events.
let maximumIndependentSubset (G : DCR.dcr) E = 
  DCR.verify_events G E
  let (g : Hare.hare)  = Hare.ofDcr G
  let T, M = g.T, g.M
  let N = Array.length T.incl
  let all  = E |> Seq.map (Hare.idxOf g) |> Bitvector.ofSeq N
  let none = Bitvector.zero N
  let indtab = 
    Array.init N (fun i -> 
      Bitvector.init N (fun j -> 
        independent (Hare.ofIdx g i) (Hare.ofIdx g j) G))

  let score xs = Bitvector.count xs
  let bound rest xs = 
    Bitvector.count rest + Bitvector.count xs
   
  let cutoffs = ref 0 
  let nodes   = ref 0
  let best = ref 0
  let solutions = ref []

  let rec loop rest xs = 

    if bound rest xs < !best then 
      cutoffs := !cutoffs + 1
    else

    nodes := !nodes + 1

    if Bitvector.isZero rest then 
      let s = score xs
      if s >= !best then 
          solutions := (xs,s) :: !solutions
          best := s
    else
      let e = Bitvector.lsb rest
      // Skip it. 
      loop (Bitvector.clr e rest) xs
      // Keep it. 
      loop (indtab.[e] &&& rest) (Bitvector.set e xs)

  let stopwatch = new System.Diagnostics.Stopwatch ()

  // iterative deepening
  let mutable hope = Seq.length G.events 
  while !solutions = [] do
    printf "Guess: %d ... " hope
    best := hope

    stopwatch.Start()
    loop all none
    stopwatch.Stop ()

    printf "%s " (if !solutions = [] then "no " else "yes") 
    let elapsed = double stopwatch.ElapsedMilliseconds / 1000.0
    printfn "[%8d nd, %8d cuts, %6.2f s, %10.2f nd/s]" 
      !nodes (!cutoffs) elapsed (double (!nodes) / elapsed)
    hope <- hope - 1

  !solutions 
  |> List.map (fun (xs, s) -> 
      let xs' = 
        xs |> Bitvector.toSetSeq
           |> Seq.map (Hare.ofIdx g)
           |> Set.ofSeq
      xs', s)


(* Normalise trace for independence. *)
let normalise evts G = 
  let indp e f = independent e f G

  let evts = List.ofSeq evts

  let rec bubble y = function 
  //| x :: x' :: xs when x = x' -> bubble y (x' :: xs)
  | x :: xs when indp x y && y > x -> x :: bubble y xs
  | xs -> y :: xs

  let rec loop = function 
  | [] -> []
  | (e :: es) as xs -> 
      let xs' = bubble e es
      if xs' = xs then 
        e :: loop es
      else 
        loop xs'

  loop evts

