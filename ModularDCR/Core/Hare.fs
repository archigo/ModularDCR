module Hare

open D

// Slightly silly, but in principle faster than the tortoise. 

type Bitvector = Bitvector.Bitvector

type relations = {
  incl : Bitvector []
  excl : Bitvector []
  resp : Bitvector []

  // cond e f  iff  f -->* e
  cond : Bitvector []
  mile : Bitvector []

  cond_relevant : Bitvector
}

type marking = 
  struct
    val exec : Bitvector
    val incl : Bitvector
    val pend : Bitvector
    new (e:Bitvector, i:Bitvector, p:Bitvector) = { exec=e; incl=i; pend=p }
  end

type hare = { 
  src  : DCR.dcr
  evts : DCR.event []
  T    : relations
  M    : marking
}

let idxOf (h : hare) e = 
  try 
    h.evts |> Array.findIndex ((=) e)
  with _ -> 
    failwithf "Unable to find index of event '%s'" e

let ofIdx (h : hare) i = 
  h.evts.[i]

let ofDcr (G : DCR.dcr) = 
  let G = DCR.flatten G
  let n = G.events |> Set.count
  let corr = G.events |> Seq.mapi (fun i e -> e, i)
  let evts = corr |> Map.ofSeq

  let tr e = Map.find e evts
  let trs = Set.map tr >> Bitvector.ofSeq n 
  let trr rel = 
    let map = 
      rel 
      |> Map.toSeq 
      |> Seq.map (fun (e,es) -> tr e, trs es) 
      |> Map.ofSeq
    Array.init n (fun i -> 
      match Map.tryFind i map with
      | None -> Bitvector.zero n
      | Some x -> x) 

  { 
    src  = G
    evts = corr |> Seq.map fst |> Array.ofSeq 
    T = 
      let cond = trr G.cond_on 
      {
        incl = trr G.incl
        excl = trr G.excl
        resp = trr G.resp
        mile = trr G.mile_on
        cond = cond
        
        cond_relevant = Array.fold Bitvector.Operators.(|||) (Bitvector.zero n) cond
      }
    M = new marking(trs G.exec, trs G.insi, trs G.pend)
  }


#if HARE_OPERATORS
open Bitvector.Operators
#endif 

#if HARE_COMPUTATIONS
open Bitvector.Computations
// Combinators are really nice, but carry a 50% overhead (!)
#endif

let inline executable (T : relations) (M : marking) = 
  let xs = Bitvector.copy M.incl
  for i in Bitvector.toSetSeq M.incl do
#if HARE_OPERATORS
    let found = 
      (not <| isZero (T.cond.[i] &&& M.incl &&& (~~~ M.exec))) ||
      (not <| isZero (T.mile.[i] &&& M.incl &&& M.pend))
#else
    let found = Bitvector.Operators.nonzero (Array.length xs) (fun j -> 
      (T.cond.[i].[j] &&& M.incl.[j] &&& (~~~ M.exec.[j])) |||
      (T.mile.[i].[j] &&& M.incl.[j] &&& M.pend.[j]))
#endif
    if found then
      Bitvector.clri i xs

  xs

let inline execute (T : relations) (M : marking) e = 
  let N = Array.length M.exec
  let exec = Bitvector.set e M.exec 

  let incl = 
#if HARE_OPERATORS
    (M.incl &&& (~~~ T.excl.[e])) ||| T.incl.[e]
#else
    Array.init N (fun i -> 
      (M.incl.[i] &&& (~~~ T.excl.[e].[i])) ||| T.incl.[e].[i])
#endif

  let pend = 
#if HARE_OPERATORS
    let p = M.pend ||| T.resp.[e]
#else
    let p = Array.init N (fun i -> 
      M.pend.[i] ||| T.resp.[e].[i])
#endif
    if not (Bitvector.isset e T.resp.[e]) then
      Bitvector.clri e p 
    p

  new marking(exec, incl, pend)

  
let inline accepting (M : marking) = 
#if HARE_OPERATORS
  isZero (M.pend &&& M.incl)
#else
  let mutable i = 0
  let mutable found = false
  let N = Array.length M.pend
  while i < N && not found do
    found <- M.pend.[i] &&& M.incl.[i] <> 0UL
    i <- i + 1
  not found
#endif



// BPM '15 notion of independence
let independent T e f = 
  let (%?) (x : 'T []) i = x.[i]
  let dj = Bitvector.disjoint
  let isset = Bitvector.isset
  let (||||) = Bitvector.Operators.(|||)
  (e <> f) && 
  // 4.2(1)
  (dj <| T.incl %? e <| T.excl %? f) && 
  (dj <| T.excl %? e <| T.incl %? f) &&
  // 4.2(2)
  (if isset f (T.resp %? e) then isset f (T.resp %? f) else true) && 
  (if isset e (T.resp %? f) then isset e (T.resp %? e) else true) && 
  // 4.4(1)
  (isset e (T.cond %? f) |> not) &&
  (isset f (T.cond %? e) |> not) &&
  // 4.4(2)
  (isset e (T.excl %? f) |> not) &&
  (isset f (T.excl %? e) |> not) &&
  (isset e (T.incl %? f) |> not) &&
  (isset f (T.incl %? e) |> not) &&
  // 4.4(3) 
  (dj <| ((T.incl %? e) |||| (T.excl %? e)) <| T.cond %? f)  &&
  (dj <| ((T.incl %? f) |||| (T.excl %? f)) <| T.cond %? e) &&
  // Milestones (not in paper)
  (isset e (T.mile %? f) |> not) &&
  (isset f (T.mile %? e) |> not) &&
  (dj <| ((T.incl %? e) |||| (T.excl %? e)) <| T.mile %? f)  &&
  (dj <| ((T.incl %? f) |||| (T.excl %? f)) <| T.mile %? e) 

let indtab (T : relations) =
  let N = Array.length T.incl
  Array.init N (fun i -> Bitvector.init N (independent T i))


// Partition the events of M,T into sets A, B, C, s.t. 
// - Every a,b in A * B are independent 
// - Every c in C conflicts with some a in A and some b in B

open Bitvector.Operators

       
(*

let partition (G : DCR.dcr) = 
  let g = ofDcr G
  let T, M = g.T, g.M
  let N = Array.length T.incl
  let all  = Bitvector.ones N
  let none = Bitvector.zero N
  let indtab = 
    Array.init N (fun i -> 
      Bitvector.init N (independent T i))
  let projtab = 
    Array.init N (fun i -> 
      G 
      |> DCR.flatten
      |> flip Concurrency.project [ofIdx g i]
      |> (fun G -> G.events)
      |> Seq.map (idxOf g)
      |> Bitvector.ofSeq N)

  let score xs n = 
    let total x = 
      x 
      |> Bitvector.toSetSeq
      |> Seq.map (fun i -> projtab.[i])
      |> Seq.fold (|||=) (Bitvector.copy x)
    let mutable s = 0
    for x in xs do 
      let y = total x &&& (~~~ x)
      for i in Bitvector.toSetSeq y do
        s <- s + Bitvector.count (indtab.[i] &&& y)
    s
   
  let cutoffs = ref 0 
  let nodes   = ref 0
  let best = ref System.Int32.MaxValue
  let solutions = ref []

  let rec loop rest (xs : Bitvector []) n = 

    (*
    if bound rest xs is n > !best then 
      cutoffs := !cutoffs + 1
    else
    *)

    nodes := !nodes + 1

    if Bitvector.isZero rest then 
      let s = score xs n
      if s <= !best then 
          solutions := (xs,s) :: !solutions
          best := s
    else
      let pushi i = Bitvector.copy xs.[i] //, is.[i]
      let popi i xsi = 
        xs.[i] <- xsi
        //is.[i] <- isi
        
      let e = Bitvector.lsb rest
      let rest' = Bitvector.clr e rest
      
      for i in 0 .. n-1 do 
        let state = pushi i
        // Add event e and its projection externals
        //xs.[e] |||= projtab.[e] |> ignore
        Bitvector.seti e xs.[e]
        // Add all new independent pairs to score
        (*
        for j in 
          Seq.append [e] 
            (Bitvector.toSetSeq (projtab.[e] &&& ~~~(fst state))) 
          do
            let delta = indtab.[j] &&& xs.[i] |> Bitvector.count
            is.[i] <- is.[i] + delta
        *)
        
        loop rest' xs n
        popi i state 

      if n+1 < N then 
        xs.[n+1] <- Bitvector.copy (Bitvector.zero N)
        Bitvector.seti e xs.[n+1]
        //is.[n+1] <- 0
        loop rest' xs (n+1)

  let stopwatch = new System.Diagnostics.Stopwatch ()

  // iterative deepening
  let mutable hope = 0 
  while !solutions = [] do
    printf "Guess: %d ... " hope
    best := hope

    let evts = Bitvector.ones N
    let is = Array.create N 0 
    let xs = Array.init N (fun i -> 
      Bitvector.copy (Bitvector.zero N))

    stopwatch.Start()
    loop evts xs 0
    stopwatch.Stop ()

    printf "%s " (if !solutions = [] then "no " else "yes") 
    let elapsed = double stopwatch.ElapsedMilliseconds / 1000.0
    printfn "[%8d nd, %8d cuts, %6.2f s, %10.2f nd/s]" 
      !nodes (!cutoffs) elapsed (double (!nodes) / elapsed)
    hope <- hope + 1

  !solutions 
  |> List.map (fun (xs, s) -> 
      let xs' = 
        xs |> Seq.map (Bitvector.toSetSeq)
           |> Seq.map (Seq.map (ofIdx g))
           |> Seq.map (Set.ofSeq)
      xs', s)
*)

let check P (h : hare) = 
  let no_threads = System.Environment.ProcessorCount + 1
  let ts = {
    TS.actions = fun M -> executable h.T M 
    TS.move    = execute h.T 
    TS.state0  = h.M
    TS.gen     = fun x -> 
      Bitvector.toSetSeq x :> System.Collections.Generic.IEnumerable<int>
  }
  TS.search no_threads ts P

let check_route dest0 using0 avoid0 (G : DCR.dcr) = 
  let g = ofDcr G
  let N = G.events |> Seq.length
  let dest  = dest0  |> Seq.map (idxOf g) |> Bitvector.ofSeq N
  let using = using0 |> Seq.map (idxOf g) |> Bitvector.ofSeq N
  let avoid = avoid0 |> Seq.map (idxOf g) |> Bitvector.ofSeq N
    
  let no_threads = System.Environment.ProcessorCount + 1
  let ts = {
    TS.actions = fun (M, _) -> executable g.T M &&& (~~~ avoid)
    TS.move    = fun (M, using) e -> execute g.T M e, Bitvector.clr e using
    TS.state0  = (g.M, using)
    TS.gen     = fun x -> 
      Bitvector.toSetSeq x :> System.Collections.Generic.IEnumerable<int>
  }
  let P (M : marking, using) _ = (not (Bitvector.isZero (dest &&& M.exec))) && Bitvector.isZero using
  let trace, stats = TS.search no_threads ts P 
  Option.map (List.map <| ofIdx g) trace

let count = check (fun _ _ -> false) >> snd

let check_event e =
  check (fun _ xs -> Bitvector.isset e xs)

let toEvents (g : hare) (x : Bitvector) = 
  Bitvector.toSetSeq x
  |> Seq.map (ofIdx g)
  |> Set.ofSeq

  
  (*
let find_partitions (G : DCR.dcr) = 
  let g = ofDcr G
  let parts = partition G |> List.ofSeq
  let best = List.fold (fun b x -> min b (score x)) System.Int32.MaxValue parts
  parts 
  |> List.filter (score >> ((=) best))
  |> List.map (fun (A,B,C) -> 
    toEvents g A, toEvents g B, toEvents g C)
*)

let impose_partition (G : DCR.dcr) (A, B, C) = 
  { DCR.flatten G with
      groups = 
        Grouping.Partition (
          [Some "Subpart A",    A, Grouping.empty;
           Some "Synchronization", C, Grouping.empty;
           Some "Subpart B",    B, Grouping.empty] 
          |> Set.ofList)
  }



let version = 
#if HARE_OPERATORS
  "OPERATORS"
#else
  "RAW"
#endif

open Bitvector.Operators
  
(*
let check_route e hit avoid h = 
  let ts = { 
    TS.actions = 
      let ok = pneg avoid 
      fun (M,_) -> executable h.T M &&&= ok
    TS.move    = fun (M,m) e -> execute h.T M e, Bitvector.clr e m
    TS.state0  = M, hit
    TS.gen     = fun x ->
      Bitvector.toSetSeq x :> System.Collections.Generic.IEnumerable<int>
  }

let check G e = 
  let h = mk G
  let i = Array.findIndex ((=) e) h.evts
  let no_threads = System.Environment.ProcessorCount + 1
  TS.search no_threads ts (fun _ xs -> Bitvector.isset i xs)
*)
