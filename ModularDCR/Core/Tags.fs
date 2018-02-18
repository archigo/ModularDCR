module Tags

open DCR

// Tags.

let add e t v G = 
  let etags = defaultArg <| Map.tryFind e G.tags <| Map.empty
  let tvals = defaultArg <| Map.tryFind t etags <| Set.empty
  { G with tags = Map.add e (Map.add t (Set.add v tvals) etags) G.tags }

let find e t G = 
  try Map.find t <| Map.find e G.tags
  with _ -> Set.empty 

let hasTag e t G = 
  find e t G <> Set.empty

let tryFindUnique e t G = 
  let ts = find e t G
  if (Set.count ts = 1) then
      ts |> Seq.head |> Some
  else 
      None

// Roles

let rolesOf G =
  Map.fold (fun rs _ ts -> 
      match Map.tryFind "role" ts with
      | None -> rs
      | Some rs' -> Set.union rs rs')
    Set.empty
    G.tags

let roleOf G e = tryFindUnique e "role" G

let eventsOfRole G role = 
  G.events |> Set.filter (fun e -> 
    find e "role" G 
    |> Set.contains role)

// True if e has role in roles
let roleFilter G roles e = 
  find e "role" G
  |> Set.intersect roles 
  |> Set.isEmpty
  |> not

