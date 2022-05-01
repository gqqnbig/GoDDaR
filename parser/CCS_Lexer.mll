{
open CCS_Parser
}

let whitespace = [' ' '\t']+
let letter = ['a'-'z']

rule read = parse
    | whitespace { Format.eprintf "Whitespace "; read lexbuf }
    | "!" { Format.eprintf "OUTPUT "; OUTPUT }
    | "?" { Format.eprintf "INPUT  "; INPUT  }
    | "+" { Format.eprintf "CHOICE "; CHOICE }
    | "|" { Format.eprintf "PAR    "; PAR    }
    | "(" { Format.eprintf "LPAREN "; LPAREN }
    | ")" { Format.eprintf "RPAREN "; RPAREN }
    | "." { Format.eprintf "PREFIX "; PREFIX }
    | letter { Format.eprintf "LABEL %c" (Lexing.lexeme_char lexbuf 0); LABEL (Lexing.lexeme_char lexbuf 0)}
    | eof { EOF }
