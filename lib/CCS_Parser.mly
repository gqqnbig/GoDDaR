%{
open Types
%}

%token <string> LABEL
%token LPAREN
%token RPAREN
%token INACTIVE
%token OUTPUT
%token INPUT
%token PREFIX
%token EXTERNAL_CHOICE
%token INTERNAL_CHOICE
%token PAR
%token STAR
%token EOF

%left INTERNAL_CHOICE
%left EXTERNAL_CHOICE
%left PAR
%left PREFIX

%start <proc> prog

%%

prog:
    | e = expr; EOF { e }

expr:
    | EOF { PNil }
    | INACTIVE { PNil }
    | s = LABEL; OUTPUT; PREFIX; e = expr { PPref(AOut(s), e) }
    | s = LABEL; INPUT; PREFIX; e = expr { PPref(AIn(s), e) }
    | LPAREN; e = expr; RPAREN { e }
    | e1 = expr; INTERNAL_CHOICE; e2 = expr { POrI(e1, e2) }
    | e1 = expr; EXTERNAL_CHOICE; e2 = expr { POrE(e1, e2) }
    | e1 = expr; PAR; e2 = expr { PPar(e1, e2) }
    | STAR; s = LABEL; INPUT; PREFIX; e = expr { PRepl(AIn(s), e) }