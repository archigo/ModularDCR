module Loader

open Microsoft.FSharp.Text.Lexing

(* I apparently do not get to insert code at the end of the fsyacc generated file. *)

exception 
    SyntaxError of string * int * int
with
    override this.Message = 
        sprintf "%s, line %d, column %d: syntax error."         
                this.Data0
                this.Data1
                this.Data2
            
let parse production fname textreader = 
    let lexbuf = LexBuffer<char>.FromTextReader textreader
    try
        production Lexer.tokenize lexbuf
        (* |> Derived.combine *)
    with 
        Failure ("parse error") ->
            raise <| SyntaxError (fname, lexbuf.EndPos.Line + 1, 
                                         lexbuf.EndPos.Column + 1)
      | _ -> reraise ()

let load origin (reader : System.IO.TextReader) = 
    match reader.Peek () with 
    | -1 -> DCR.empty
    | 0x3c -> XML.deserialize reader |> fst  // 0x3c is '<' in most character encodings. 
    | _ -> parse Parser.decls origin reader

let loadfile file = 
    use r = new System.IO.StreamReader(file : string)
    load file r

let loadstr str = 
    use r = new System.IO.StringReader(str)
    load "(stdin)" r

let parse_event G str = 
    use sr = new System.IO.StringReader(str)
    let action = parse Parser.action "(stdin)" sr
    match action with 
    | DCR.Event e -> [e]
    | DCR.Recv (e, args) -> [e]
    | DCR.Sync (e, f) -> [e; f]
    |> List.iter (fun e -> DCR.verify_event G e)
    action

let parse_label str = 
    use sr = new System.IO.StringReader(str)
    parse Parser.label "(stdin)" sr
