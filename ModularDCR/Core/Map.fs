module Map

    type powermap<'Key,'T when 'Key : comparison and 'T : comparison> 
        = Map<'Key, Set<'T>>

    let insertMany k vs m = 
        let vs'' = 
            match Map.tryFind k m with
            | None -> vs
            | Some vs' -> Set.union vs vs'
        Map.add k vs'' m
        
    let insert k v = insertMany k (Set.singleton v) 

    let lookup k m =
        match Map.tryFind k m with
        | Some ts -> ts
        | None -> Set.empty

    let merge combine (f : Map<'a, 'T>) (g : Map<'a, 'T>) = 
        let gt, lt = if f.Count > g.Count then f,g else g,f
        lt |> Map.fold (fun r e l ->
                          match Map.tryFind e gt with
                          | None -> Map.add e l r
                          | Some l' -> Map.add e (combine e l l') r)
              gt

    let oplus f g = 
      let combine e l l' = 
        if l = l' then 
          l 
        else 
          failwithf "Disjoint union of maps failed at key '%s' ('%A' <> '%A')" e l l'
      merge combine f g 

    let union (f : Map<'a, Set<'b>>) (g : Map<'a,Set<'b>>) =
        merge (fun _ fs fs' -> fs |> Set.union <| fs') f g

    let transform f m = 
        m |> Map.toSeq |> Seq.map f |> Map.ofSeq
