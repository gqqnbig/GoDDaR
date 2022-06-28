
let parse (s) =
    let lexbuf = Lexing.from_string s in
    let ast = MiGo_Parser.prog MiGo_Lexer.read lexbuf in
    ast


let parse_file file =
    let c = open_in file in 
    let lexbuf = Lexing.from_channel c in
    MiGo_Parser.prog MiGo_Lexer.read lexbuf