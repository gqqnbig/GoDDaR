{
    open MiGo_Parser
    let fmt = Format.std_formatter
}

let whitespace = [' ' '\t']+
let newline = ['\n']+
let identifier = ['a'-'z' 'A'-'Z' '0'-'9' '.' '$' '#' '_']+
let tag = ['a'-'z' 'A'-'Z' '0'-'9' '.' '$' '#' '_' ':' '/' '-']+
let digit      = ['0'-'9']+

rule read = parse
    | whitespace { (* Format.fprintf fmt "Whitespace ";         *) read lexbuf }
    | newline    { (* Format.fprintf fmt "Newline ";            *) Lexing.new_line lexbuf; read lexbuf }
    | "def"      { (* Format.fprintf fmt "DEF ";                *) DEFINITION }
    | "("        { (* Format.fprintf fmt "LPAREN ";             *) LPAREN }
    | ")"        { (* Format.fprintf fmt "RPAREN ";             *) RPAREN }
    | ":"        { (* Format.fprintf fmt "COLON ";              *) COLON }
    | ";"        { (* Format.fprintf fmt "SEMICOLON ";          *) SEMICOLON }
    | ","        { (* Format.fprintf fmt "COMMA ";              *) COMMA }
    | "send"     { (* Format.fprintf fmt "SEND ";               *) SEND }
    | "recv"     { (* Format.fprintf fmt "RECEIVE ";            *) RECEIVE }
    | "tau"      { (* Format.fprintf fmt "TAU ";                *) TAU }
    | "let"      { (* Format.fprintf fmt "LET ";                *) LET }
    | "="        { (* Format.fprintf fmt "EQUALS ";             *) EQUALS }
    | "newchan"  { (* Format.fprintf fmt "NEWCHAN ";            *) NEWCHAN }
    | "close"    { (* Format.fprintf fmt "CLOSE ";              *) CLOSE }
    | "call"     { (* Format.fprintf fmt "CALL ";               *) CALL }
    | "spawn"    { (* Format.fprintf fmt "SPAWN ";              *) SPAWN }
    | "if"       { (* Format.fprintf fmt "IF ";                 *) IF }
    | "else"     { (* Format.fprintf fmt "ELSE ";               *) ELSE }
    | "endif"    { (* Format.fprintf fmt "ENDIF ";              *) ENDIF }
    | "select"   { (* Format.fprintf fmt "SELECT ";             *) SELECT }
    | "case"     { (* Format.fprintf fmt "CASE ";               *) CASE }
    | "endselect"{ (* Format.fprintf fmt "ENDSELECT ";          *) ENDSELECT }
    | "lock"[^'\n']*'\n'   { (* Format.fprintf fmt "LOCK ";               *) read lexbuf }
    | "unlock"[^'\n']*'\n' { (* Format.fprintf fmt "UNLOCK ";             *) read lexbuf }
    | "letmem"[^'\n']*'\n' { (* Format.fprintf fmt "LETMEM ";             *) read lexbuf }
    | "write"[^'\n']*'\n'  { (* Format.fprintf fmt "WRITE ";              *) read lexbuf }
    | "read"[^'\n']*'\n'   { (* Format.fprintf fmt "READ ";               *) read lexbuf }
    | "--"[^'\n']*'\n' { (* Format.fprintf fmt "COMMENT ";      *) Lexing.new_line lexbuf; read lexbuf }
    | digit      { (* Format.fprintf fmt "DIGIT ";              *) DIGIT (int_of_string (Lexing.lexeme lexbuf)) }
    | identifier { (* Format.fprintf fmt "IDENTIFIER %s " (Lexing.lexeme lexbuf); *) IDENTIFIER (Lexing.lexeme lexbuf) }
    | tag        { (* Format.fprintf fmt "TAG %s " (Lexing.lexeme lexbuf); *) TAG (Lexing.lexeme lexbuf) }
    | eof        { (* Format.fprintf fmt "EOF ";                *) EOF }
