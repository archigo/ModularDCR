module D 

  // Debugging 
  let dbg x = printfn "%A" x
              x

  // Combinators
  let flip f x y = f y x 
  let tee  fi x = fi x ; x

  // Map helpers 
  let safeFind k m = 
    match Map.tryFind k m with
    | Some v -> v
    | None -> failwithf "%A\n\n*** Lookup failed for key '%s'." m k

  // Errors

  type Err (errno, strerr) = 
    inherit System.Exception(sprintf "[%04d] %s" errno strerr)
    with 
      member __.Code        = errno
      member __.Description = strerr

  let error n fmt = 
      Printf.kprintf (fun s -> raise <| Err (n, s)) fmt

