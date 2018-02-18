#r "bin/debug/Core.dll"

let G0 = 
  Loader.loadstr """
    limit ? { A -->* B when limit > 1000 }
  """

printfn "%A" <| DCR.executable G0

let d1 = 900 
let G1  = DCR.execute_with_instantiation "limit" [ Data.Int d1 ] G0
printfn "%d: %A" d1 <| DCR.executable G1

let d2 = 1100 
let G2  = DCR.execute_with_instantiation "limit" [ Data.Int d2 ] G1
printfn "%d: %A" d2 <| DCR.executable G2




