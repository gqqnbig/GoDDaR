let parse (s) : Types.proc =
    let lexbuf = Lexing.from_string s in
    try 
        CCS_Parser.prog CCS_Lexer.read lexbuf
    with
      | _ ->
        let position = Lexing.lexeme_start_p lexbuf in
        let pos_string = Format.sprintf "lineNum:Char %d:%d" position.pos_lnum (position.pos_cnum - position.pos_bol) in
        Format.printf "%s\n" pos_string;
        failwith pos_string