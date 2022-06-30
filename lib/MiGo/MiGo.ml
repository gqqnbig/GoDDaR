let do_parse lexbuf =
    try 
        MiGo_Parser.prog MiGo_Lexer.read lexbuf
    with
      | _ ->
        let position = Lexing.lexeme_start_p lexbuf in
        Format.printf "\nlineNum:Char %d:%d\n" position.pos_lnum (position.pos_cnum - position.pos_bol); failwith ""
let parse (s) =
    let lexbuf = Lexing.from_string s in
    do_parse lexbuf

let parse_file file =
    let c = open_in file in 
    let lexbuf = Lexing.from_channel c in
    do_parse lexbuf
