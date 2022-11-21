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

%start <lambda> prog

%%

prog:
    | e = expr; EOF { e }

expr:
    | EOF { LNil }
    | INACTIVE { LNil }
    | s = LABEL; OUTPUT; PREFIX; e = expr { LList(EEta(AOut(s)), e) }
    | s = LABEL; INPUT; PREFIX; e = expr { LList(EEta(AIn(s)), e) }
    | LPAREN; e = expr; RPAREN { e }
    | e1 = expr; INTERNAL_CHOICE; e2 = expr { LOrI(e1, e2) }
    | e1 = expr; EXTERNAL_CHOICE; e2 = expr { LOrE(e1, e2) }
    | e1 = expr; PAR; e2 = expr { LPar(e1, e2) }
    | STAR; s = LABEL; INPUT; PREFIX; e = expr { LRepl(EEta(AIn(s)), e) }