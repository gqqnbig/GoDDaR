{
open CCS_Parser
}

let whitespace = [' ' '\t']+
let identifier = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

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
    | "*"        { (* Format.eprintf "STAR ";                *) STAR }
    | identifier { (* Format.eprintf "LABEL %c " (Lexing.lexeme lexbuf); *) LABEL (Lexing.lexeme lexbuf)}
    | eof        { EOF }
