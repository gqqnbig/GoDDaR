%{
open Dlock.Types
%}

%token <char> LABEL
%token OUTPUT
%token INPUT
%token CHOICE
%token PAR
%token LPAREN
%token RPAREN
%token PREFIX
%token EOF

%left PREFIX
%left PAR
%left CHOICE

%start <proc> prog

%%

prog:
    | e = expr; EOF { e }

expr:
    | EOF { PNil }
    | s = LABEL; e = expr { PPref(AOut(s), e) }
(*
    | EOF { PNil }
    | s = LABEL; OUTPUT; PREFIX; e = expr { PPref(AOut(s), e) }
    | s = LABEL; INPUT; PREFIX; e = expr { PPref(AIn(s), e) }
    | LPAREN; e = expr; RPAREN { e }
    | e1 = expr; CHOICE; e2 = expr { POr(e1, e2) }
    | e1 = expr; PAR; e2 = expr { PPar(e1, e2) }
*)