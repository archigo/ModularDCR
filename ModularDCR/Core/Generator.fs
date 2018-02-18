module Generator

let generate_trace p (rand : System.Random) (G : DCR.dcr) =
  let rec loop G trace = 
    if 
      not (List.isEmpty trace) 
      && DCR.accepting G 
      && rand.NextDouble() > p 
    then 
      List.rev trace
    else
      let xs = DCR.executable G
      let n = xs.Count
      let e = Seq.item (rand.Next(0, n)) xs
      loop (DCR.execute e G) (DCR.show_label G e :: trace)
  loop G [] 
 

open System.IO
open System.Runtime.Serialization.Json
open System.Text

let to_json<'T> (x:'T) = 
  // TODO: For large data sets, converting to an internal string before output
  // is probably unwise. However, I don't have the time to figure out how to 
  // convert between Stream and TextWriter. 
  use ms = new MemoryStream() 
  (new DataContractJsonSerializer(typeof<'T>)).WriteObject(ms, x) 
  Encoding.Default.GetString(ms.ToArray()) 


let generate_log expected_length seed n (G : DCR.dcr) =
  let rand = 
    match seed with 
    | Some x -> System.Random(x)
    | None -> System.Random()
  let p = 
    match expected_length with
    | 0 -> -1.0
    | n -> 0.5 ** (1.0 / (float n))
  
  let json = DataContractJsonSerializer(typeof<string []>)
    
  Seq.initInfinite (fun _ -> generate_trace p rand G)
    |> Seq.take n 
    |> Seq.map Array.ofList
    |> Array.ofSeq
    |> to_json
 
  
