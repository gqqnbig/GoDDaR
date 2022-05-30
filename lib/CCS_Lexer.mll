{
open CCS_Parser
}

let whitespace = [' ' '\t']+
let letter = ['a'-'z']

rule read = parse
    | whitespace { (* Format.eprintf "Whitespace ";          *) read lexbuf }
    | "0"        { (* Format.eprintf "INACTIVE ";            *) INACTIVE }
    | "!"        { (* Format.eprintf "OUTPUT ";              *) OUTPUT }
    | "?"        { (* Format.eprintf "INPUT  ";              *) INPUT  }
    | "+"        { (* Format.eprintf "INTERNAL_CHOICE ";     *) INTERNAL_CHOICE }
    | "&"        { (* Format.eprintf "EXTERNAL_CHOICE ";     *) EXTERNAL_CHOICE }
    | "||"       { (* Format.eprintf "PAR    ";              *) PAR    }
    | "("        { (* Format.eprintf "LPAREN ";              *) LPAREN }
    | ")"        { (* Format.eprintf "RPAREN ";              *) RPAREN }
    | "."        { (* Format.eprintf "PREFIX ";              *) PREFIX }
    | letter     { (* Format.eprintf "LABEL %c " (Lexing.lexeme_char lexbuf 0); *) LABEL (Lexing.lexeme_char lexbuf 0)}
    | eof        { EOF }
