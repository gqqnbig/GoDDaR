type migo_def = 
  | Def of string * string list * migo_stmt list
and  migo_stmt = 
  | Prefix of migo_prefix
  | Newchan of string * string * int
  | Close of string
  | Call of string * string list
  | Spawn of string * string list
  | If of migo_stmt list * migo_stmt list
  | Select of (migo_prefix * migo_stmt list) list
  | Lock of string
  | Unlock of string
and migo_prefix = 
  | Send of string * string
  | Receive of string * string
  | Tau

let indentation_space_multiplier = 4
let print_migo_prefix fmt prefix = 
  match prefix with
  | Send(c, tag)    -> Format.fprintf fmt "send %s" c; if tag <> "" then Format.fprintf fmt " (%s)" tag;
  | Receive(c, tag) -> Format.fprintf fmt "recv %s" c; if tag <> "" then Format.fprintf fmt " (%s)" tag;
  | Tau -> Format.fprintf fmt "tau"

let rec print_migo_stmts fmt indent stmts: unit =
  List.iter (print_migo_stmt fmt indent) stmts
and print_migo_stmt indent fmt stmt: unit = 
  let line_prefix = (String.make (indent*indentation_space_multiplier)  ' ') in
  Format.fprintf fmt "%s" line_prefix;
  match stmt with
  | Prefix(prefix) ->
    Format.fprintf fmt "%a;\n" print_migo_prefix prefix
  | Newchan(c, type_string, capacity) ->
    Format.fprintf fmt "let %s = newchan %s, %i;\n" c type_string capacity
  | Close(c) ->
    Format.fprintf fmt "close %s;\n" c
  | Call(name, params) ->
    Format.fprintf fmt "call %s(%s);\n" name (String.concat ", " params)
  | Spawn(name, params) ->
    Format.fprintf fmt "spawn %s(%s);\n" name (String.concat ", " params)
  | If(t, f) ->
    Format.fprintf fmt "if\n%a%selse\n%a%sendif;\n"
      (print_migo_stmts (indent+1)) t
      line_prefix
      (print_migo_stmts (indent+1)) f
      line_prefix
  | Select(list) ->
    Format.fprintf fmt "select\n";
    List.iter (
      fun (prefix, stmts) ->
        Format.fprintf fmt "%scase %a;\n" (line_prefix ^ "  ") print_migo_prefix prefix;
        print_migo_stmts (indent+1) fmt stmts
    ) list;
    Format.fprintf fmt "%sendselect;\n" line_prefix
  | Lock(c) ->
    Format.fprintf fmt "lock %s;\n" c
  | Unlock(c) ->
    Format.fprintf fmt "unlock %s;\n" c

let print_migo fmt indent (migo: migo_def) =
  Format.fprintf fmt "%s" (String.make (indent*indentation_space_multiplier) ' ');
  match migo with
  | Def(id, params, stmts) ->
    Format.fprintf fmt "def %s(%s):\n" id (String.concat ", " params);
    print_migo_stmts (indent+1) fmt stmts

let print_migo_list fmt (migo_list: migo_def list) =
  let rec do_print_migo_list (migo_list: migo_def list) =
    match migo_list with
    | [] -> ()
    | hd::[] -> (print_migo fmt 0 hd)
    | hd::tl -> (print_migo fmt 0 hd; do_print_migo_list tl)
  in
    do_print_migo_list migo_list
