%{
open MiGo_Types
%}

%token <string> IDENTIFIER
%token <string> TAG
%token <int> DIGIT
%token DEFINITION
%token LPAREN
%token RPAREN
%token COLON
%token SEMICOLON
%token COMMA
%token SEND
%token RECEIVE
%token TAU
%token LET
%token EQUALS
%token NEWCHAN
%token CLOSE
%token CALL
%token SPAWN
%token IF
%token ELSE
%token ENDIF
%token SELECT
%token CASE
%token ARROW
%token ENDSELECT
%token EOF

%start <migo_def list> prog

%%

prog:
    | e = definition; EOF {[e]}
    | e = definition; p = prog {e::p}

definition:
    | DEFINITION; id = IDENTIFIER; LPAREN; p = param; RPAREN; COLON; body = def_body { Def(id, List.rev p, body) } 

param:
    | e = params {e}
    | { [] }

params:
    | id = IDENTIFIER { [id] }
    | p = params; COMMA; id = IDENTIFIER { id::p }

def_body: 
    | e = def_stmt {[e]}
    | e = def_stmt; p = def_body {e::p}

def_body2: 
    | {[]}
    | e = def_stmt; p = def_body2 {e::p}

prefix:
    | SEND; id = IDENTIFIER; tag = prefix_tag { Send(id, tag) }
    | RECEIVE; id = IDENTIFIER; tag = prefix_tag; deps = prefix_deps { Receive(id, tag, deps) }
    | TAU { Tau }

prefix_tag:
    | LPAREN; tag = TAG; RPAREN { tag }
    | { "" }

prefix_deps:
    | ARROW; deps = prefix_deps_list; {deps}
    | { [] }

prefix_deps_list:
    | LPAREN; tag = TAG; RPAREN; tags = prefix_deps_list { tag::tags }
    | { [] }

def_stmt:
    | p = prefix; SEMICOLON { Prefix(p) }
    | LET; chanid = IDENTIFIER; EQUALS; NEWCHAN; typeid = IDENTIFIER; COMMA; capacity = DIGIT; SEMICOLON {Newchan(chanid, typeid, capacity)}
    | CLOSE; id = IDENTIFIER; SEMICOLON {Close(id)}
    | CALL; id = IDENTIFIER; LPAREN; p = param; RPAREN; SEMICOLON {Call(id, List.rev p)}
    | SPAWN; id = IDENTIFIER LPAREN; p = param; RPAREN; SEMICOLON {Spawn(id, List.rev p)}
    | IF; t = def_body2; ELSE; f = def_body2; ENDIF; SEMICOLON {If(t, f)}
    | SELECT; c = cases; ENDSELECT; SEMICOLON {Select(c)}

cases: 
    | e = case {[e]}
    | e = case; p = cases {e::p}
case: 
    | CASE; p = prefix; SEMICOLON; e = def_body2 {(p, e)}