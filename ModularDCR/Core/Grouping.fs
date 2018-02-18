module Grouping

open D

// A grouping implements nesting in the sense of 
//   R. R. Mukkamala, T. T. Hildebrandt & T. Slaats:
//   "Nested Dynamic Condition Response Partitionaphs",
//   FSEN '11
// 
// We call it a "grouping" rather than a nesting because non-leaf events are
// not actual events (can't fire). 
//
// In this implementation, we sonder between actual events (leafs), here just
// 'T, and internal nodes, here 'name.

(* Auxiliary type representing the grouping, a desired nesting of events in
 * boxes. Technically, a grouping is a partitioning of a set of nodes, with
 * each partition recursively containing sub-groupings (partitions). *)
type partition<'T, 'name when 'T : comparison and 'name : comparison> =
  Partition of Set<Option<'name> * Set<'T> * partition<'T, 'name>>

let empty = Partition Set.empty

// Determine 'Ts in partition. 
let domain (Partition p) = 
  p |> Set.fold (fun a (_, es, _) -> Set.union a es) Set.empty

// Determine names in partition.
let rec names (Partition pa) = 
  seq {
    for (name, _, pa') in pa do
      if Option.isSome name then 
        yield Option.get name
      yield! names pa'
  }
 
let tryFind t (Partition p) = 
  p |> Seq.tryFind (fun (_, ts, _) -> Set.contains t ts)

let rec map' f (Partition p) = 
  p |> Set.map (fun (name, es, P) -> f (name, es, map' f P))
    |> Partition

let rec map f = map' (fun (name, es, P) -> (name, f es, P))

let rec removeEmpty pa =
  let rec loop (Partition p) = 
    let processed = 
      p 
      |> Set.map (fun (name, elems, p') -> 
          if Set.isEmpty elems then
            None
          else
            let pa' = 
              match loop p' with
              | None -> Partition Set.empty
              | Some pa -> pa
            Some (name, elems, pa'))
      |> Set.filter (Option.isSome)
      |> Set.map (Option.get)
    if Set.isEmpty processed then 
      None
    else 
      Some <| Partition processed
  
  match loop pa with
  | None -> Partition Set.empty
  | Some p -> p

let verify_non_overlapping g h =
    let overlap = Set.intersect (domain g) (domain h)
    if overlap <> Set.empty then
        error 0022 "Incompatible grouping ('%A')."
                   (overlap |> Set.toSeq |> Seq.pick Some)

let merge (Partition p as P) (Partition q as Q) =
    verify_non_overlapping P Q
    Set.union p q |> Partition

// Insert new h into g at the sub-partition containing t as leaf
let add P0 t Q =
  verify_non_overlapping P0 Q 
  let Qdom = domain Q
  let rec loop (Partition ps as P) =
    if not <| Set.exists (fun (_, ts, _) -> Set.contains t ts) ps then
      merge P Q
    else
      ps 
      |> Set.map (fun ((name, ts, P') as P'') -> 
        if Set.contains t ts then 
          (name, Set.union ts Qdom, loop P')
        else
          P'')
      |> Partition
  loop P0 

let rec tryFindGroup (Partition p) name = 
  p 
  |> Set.toSeq
  |> Seq.tryPick (fun ((n, ts, P) as g) -> 
    if n = Some name then
      Some g
    else
      tryFindGroup P name)


let ancestors P0 x = 
  let rec forLeaf path P = 
    match tryFind x P with 
    | None -> List.rev (x :: path)
    | Some (Some n, _, P') -> forLeaf (n :: path) P'
    | Some (_, _, P') -> forLeaf path P'

  let rec forName path (Partition p) = 
    p 
    |> Set.toSeq
    |> Seq.tryPick (fun (n, ts, P) -> 
      match n with 
      | Some n' when n' = x -> 
          List.rev (x :: path) |> Some
      | Some n' -> forName (n' :: path) P
      | _ -> None) 

  match Seq.tryFind ((=) x) <| names P0 with
  | None -> forLeaf [] P0
  | Some _ -> 
      match forName [] P0 with
      | None -> failwithf "Can't compute ancestors of non-existent group '%A'" x
      | Some xs -> xs

  |> List.toSeq

let descendants P0 x = 
  match tryFindGroup P0 x with
  | Some (_, ts, _) -> ts |> Set.toSeq
  | None -> Seq.singleton x

    
// Find all x s.t. for some ancestors n of t and m of x, n rel m.
//
//       n   rel   m
//       ^         ^
//       |         | 
//       |         | 
//       t         x
//
// Ancestor-relation is here understood to be reflexive. 
//
// TODO: I'm unhappy that I don't have better distinction
//       between names and events in DCR graphs. We can see
//       that here in that bridge accepts only partitions of type
//       <'a, 'a>, not <'a, 'b>
//
let bridge P0 t rel = 
    let isName = flip Set.contains (names P0 |> Set.ofSeq)
    // TODO: Maybe names should be pre-computed in the partition?

    let conames, cots_direct = 
      ancestors P0 t 
      |> Seq.collect rel 
      |> Set.ofSeq 
      |> Set.partition isName

    let cots_trans = 
      conames 
      |> Seq.collect (descendants P0)
      |> Set.ofSeq

    Set.union cots_direct cots_trans

