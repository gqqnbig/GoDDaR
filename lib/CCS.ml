let parse (s) : Types.proc =
    let lexbuf = Lexing.from_string s in
    let ast = CCS_Parser.prog CCS_Lexer.read lexbuf in
    ast