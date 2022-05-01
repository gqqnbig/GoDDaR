open Dlock.Types

let parse (s) : proc =
    let lexbuf = Lexing.from_string s in
    let ast = CCS_Parser.prog CCS_Lexer.read lexbuf in
    ast