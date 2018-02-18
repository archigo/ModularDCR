module Diagrams

type output_format = 
  | PDF 
  | SVG  
  | DOT
  override __.ToString() = 
    // NB! Used as filename extensions. 
    match __ with 
      | PDF -> "pdf"
      | SVG -> "svg"
      | DOT -> "dot"

module Types = 
  let dcr = Dot.mk_dot
  let net = ModelChecking.mk_net_dot
  let tsr = ModelChecking.mk_tsr_dot

let toDiagramFile (fname : string) format artist G =
  match format with
  | DOT -> 
      use ostr = new System.IO.StreamWriter(fname)
      artist G ostr 
  | _ -> 
    Dot.compileDotFile (format.ToString()) (artist G) fname

let toDiagramString (format : output_format) artist G = 
  Dot.compileDotString (format.ToString()) (artist G)

// Convenience class for dcri
type Visualiser (format : output_format) = 
  let _format = format
  let _outname = 
    System.IO.Path.ChangeExtension(
      System.IO.Path.GetTempFileName(),
      _format.ToString()) 

  interface System.IDisposable with
    member viz.Dispose() = 
      try 
        System.IO.File.Delete _outname
      with
         _ -> eprintfn "Unable to delete temporary file '%s'" _outname 

  member val Diagram = Types.dcr Dot.default_options_DCR with get, set

  member __.draw G = 
    toDiagramFile _outname _format __.Diagram G
    System.Diagnostics.Process.Start(_outname) |> ignore


// Convenience function for web-services
let toSVG = toDiagramString SVG (Types.dcr Dot.default_options_DCR)


let flowchartify (G : DCR.dcr) file = 
  let net = 
    Types.net {
      suppress_idems = ModelChecking.All
      suppress_as    = ModelChecking.Node
      traversal      = ModelChecking.traverse_all
    }
  let dcr = 
    Types.dcr { 
      Dot.default_options_DCR
        with
          direction = Dot.LR }

  use t = new System.IO.StreamWriter(file : string)
  fprintfn t "<html><body><h1>Split</h1><table border='1'>"
  for role in Tags.rolesOf G do
    printfn "%s" role
    let Gd = Tags.eventsOfRole G role |> Concurrency.project G  
    let diag1 = Gd |> toDiagramString SVG net
    let diag2 = Gd |> toDiagramString SVG dcr
    fprintfn t "<tr><td>%s</td><td>%s</td><td>%s</td>" 
      role diag1 diag2
    t.Flush()
  fprintfn t "</table><h1>Full</h1>%s<p><br>%s</body>"
      (toDiagramString SVG net G)
      (toDiagramString SVG dcr G)

