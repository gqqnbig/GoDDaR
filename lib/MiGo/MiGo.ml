let do_parse lexbuf =
    try 
        MiGo_Parser.prog MiGo_Lexer.read lexbuf
    with
      | e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
          Printf.eprintf "there was an error: %s%s\n" msg stack;

        let position = Lexing.lexeme_start_p lexbuf in
        let pos_string = Format.sprintf "%s: lineNum:Char %d:%d" position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol) in
        Format.printf "%s\n" pos_string;
        failwith pos_string
        

let parse (s) =
    let lexbuf = Lexing.from_string s in
    do_parse lexbuf

let parse_file file =
    let c = open_in file in 
    let lexbuf = Lexing.from_channel c in
    Lexing.set_filename lexbuf file;
    do_parse lexbuf
