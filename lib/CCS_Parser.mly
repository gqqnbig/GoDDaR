%{
open Types
%}

%token <char> LABEL
%token LPAREN
%token RPAREN
%token INACTIVE
%token OUTPUT
%token INPUT
%token PREFIX
%token CHOICE
%token PAR
%token EOF

%left CHOICE
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
    | e1 = expr; CHOICE; e2 = expr { POr(e1, e2) }
    | e1 = expr; PAR; e2 = expr { PPar(e1, e2) }