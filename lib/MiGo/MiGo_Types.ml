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
and migo_prefix = 
  | Send of string
  | Receive of string
  | Tau

let print_migo_prefix fmt prefix = 
  match prefix with
  | Send(c)    -> Format.fprintf fmt "send %s" c
  | Receive(c) -> Format.fprintf fmt "recv %s" c
  | Tau -> Format.fprintf fmt "tau"

let rec print_migo_stmts fmt stmts: unit =
  List.iter (print_migo_stmt fmt) stmts
and print_migo_stmt fmt stmt: unit = 
  match stmt with
  | Prefix(prefix) -> Format.fprintf fmt "%a;\n" print_migo_prefix prefix
  | Newchan(c, type_string, capacity) -> Format.fprintf fmt "let %s = newchan %s, %i;\n" c type_string capacity
  | Close(c) -> Format.fprintf fmt "close %s;\n" c
  | Call(name, params) -> Format.fprintf fmt "call %s(%s);\n" name (String.concat ", " params)
  | Spawn(name, params) -> Format.fprintf fmt "spawn %s(%s);\n" name (String.concat ", " params)
  | If(t, f) -> Format.fprintf fmt "if\n%a\nelse\n%a\nendif;\n" print_migo_stmts t print_migo_stmts f
  | Select(list) -> List.iter (
      fun (prefix, stmts) ->
        Format.fprintf fmt "case %a:" print_migo_prefix prefix;
        print_migo_stmts fmt stmts
    ) list

let print_migo fmt (migo: migo_def) =
  match migo with
  | Def(id, params, stmts) ->
    Format.fprintf fmt "def %s(%s):\n" id (String.concat ", " params);
    print_migo_stmts fmt stmts

let print_migo_list fmt (migo_list: migo_def list) =
  let rec do_print_migo_list (migo_list: migo_def list) =
    match migo_list with
    | [] -> ()
    | hd::[] -> (print_migo fmt hd; Format.fprintf fmt "\n")
    | hd::tl -> (print_migo fmt hd; Format.fprintf fmt "\n"; do_print_migo_list tl)
  in
    do_print_migo_list migo_list
