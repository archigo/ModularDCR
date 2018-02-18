module TS

open D
open System.Collections.Concurrent

// Action-deterministic transition system. 
type TS<'state, 'action, 'actions> = {
  actions : 'state -> 'actions 
  move    : 'state -> 'action -> 'state
  state0  : 'state
  // Auxiliary; for when state types are primitive 
  gen     : 'actions -> seq<'action>
}

type stats = {
  no_states         : int
  no_trans          : int
  elapsed           : double
  no_threads        : int
  states_per_thread : seq<int> option
} with
  member stats.states_per_second = double stats.no_states / stats.elapsed
  member stats.trans_per_second  = double stats.no_trans  / stats.elapsed
  member stats.branch_factor     = double stats.no_trans / double stats.no_states
  override __.ToString () =
    sprintf 
      "%d st, %d tr, %.2f tr/st, %.2f st/s, %.2f tr/s, %d thr %s" 
      __.no_states __.no_trans __.branch_factor
      __.states_per_second __.trans_per_second
      __.no_threads
      (match __.states_per_thread with
       | None -> ""
       | Some is -> "[" + (is |> Seq.map string |> String.concat ", ") + "]")


type Counter () = 
  let mutable count = 0
  member __.value = count
  member __.inc () = 
    System.Threading.Interlocked.Increment (&count) |> ignore


// Find a path in TS to a state satisfying P; search concurrently on at most
// no_threads threads. 
let inline search no_threads ts P = 

  let edges   = new Counter ()
  let queue   = new ConcurrentQueue<_>()
  let visited = new ConcurrentDictionary<_,_>()
  let found   = ref false

  queue.Enqueue (ts.state0, [])

  let rec bfs k = 

    if !found then None, k else 

    match queue.TryDequeue () with 
    | false, _ -> None, k 
    | true, (state, path) -> 

    edges.inc ()

    match visited.TryAdd (state, ()) with
    | false -> bfs (k+1)
    | true -> 
    
    let actions = ts.actions state 

    if P state actions then 
      found := true
      Some <| List.rev path, k
    else 
      for action in ts.gen actions do
        queue.Enqueue (ts.move state action, action :: path)
      bfs (k+1) 

  let stopwatch = new System.Diagnostics.Stopwatch ()
  let worker = 
    async { 
      return bfs 0 
    } 
  let comp = 
    Array.create no_threads worker
    |> Async.Parallel

  stopwatch.Start ()
  let results = Async.RunSynchronously comp
  stopwatch.Stop ()

  let result = 
    let paths = results |> Seq.choose fst 
    if Seq.isEmpty paths then None
    else paths |> Seq.minBy List.length |> Some

  let stats = {
    no_states         = visited.Count + 1
    no_trans          = edges.value
    elapsed           = double stopwatch.ElapsedMilliseconds / 1000.0 
    states_per_thread = results |> Seq.map snd |> Some
    no_threads        = no_threads
  }

#if DEBUG
  printfn "\n\t%s" <| stats.ToString()
#endif

  result, stats



let dfs ts P = 

  let edges   = new Counter ()
//  let stack   = new ConcurrentStack<_>()
  let visited = new ConcurrentDictionary<_,_>()
  let found   = ref false

 // stack.Push ts.state0

  let rec loop (path, state) = 

    if !found then None else 

(*
    match stack.TryPop() with 
    | false, _ -> None, k 
    | true, state -> 
*)
    edges.inc ()

    match visited.TryAdd (state, ()) with
    | false -> None
    | true -> 
    
    let actions = ts.actions state 

    if P state actions then 
      found := true
      Some <| List.rev path
    else 
      ts.gen actions 
      |> Seq.tryPick (fun action -> 
           loop (action :: path, ts.move state action))
        
  let stopwatch = new System.Diagnostics.Stopwatch ()
  let worker = 
    async { 
      return loop ([], ts.state0) 
    } 
  let comp = worker
  (*
    Array.create no_threads worker
    |> Async.Parallel
    *)

  stopwatch.Start ()
  let results = Async.RunSynchronously comp
  stopwatch.Stop ()

(*
  let result = 
    let paths = results |> Seq.choose fst 
    if Seq.isEmpty paths then None
    else paths |> Seq.minBy List.length |> Some
    *)
  let result = results

  let stats = {
    no_states         = visited.Count + 1
    no_trans          = edges.value
    elapsed           = double stopwatch.ElapsedMilliseconds / 1000.0 
    states_per_thread = None //results |> Seq.map snd |> Some
    no_threads        = 1
  }

#if DEBUG
  printfn "\n\t%s" <| stats.ToString()
#endif

  result, stats


