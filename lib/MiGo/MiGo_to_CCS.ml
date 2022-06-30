exception Fail of string

type stack_entry = string * (MiGo_Types.migo_stmt list)

let debug_fmt = Printer.null_fmt

let migo_to_ccs (migo_defs: MiGo_Types.migo_def list): string = 
  let def_hashtbl = Hashtbl.create (List.length migo_defs) in
  List.iter (
    fun (MiGo_Types.Def(name, params, stmts) as def) -> (
      Hashtbl.add def_hashtbl name def
    )
  ) migo_defs;
  let Def(_, params, stmts) as main = (Hashtbl.find def_hashtbl "main.main") in
  assert (params = []);
  let rec do_migo_to_ccs (stack: stack_entry list): string =
    match stack with 
    | [] -> Format.fprintf debug_fmt "No more stack\n"; "0"
    | (fun_name, stmts)::tl ->
      match stmts with
      | [] -> Format.fprintf debug_fmt "No stmts\n"; do_migo_to_ccs (tl)
      | stmt::stmt_tl ->
        match stmt with
        | MiGo_Types.Prefix(Send(c)) ->
          Format.sprintf "%s!.%s" c (do_migo_to_ccs ((fun_name, stmt_tl)::tl))
        | MiGo_Types.Prefix(Receive(c)) ->
          Format.sprintf "%s?.%s" c (do_migo_to_ccs ((fun_name, stmt_tl)::tl))
        | MiGo_Types.If(t, f) -> 
          Format.sprintf "(%s + %s)" (do_migo_to_ccs ((fun_name, t@stmt_tl)::tl)) (do_migo_to_ccs ((fun_name, f@stmt_tl)::tl))
        | MiGo_Types.Call(call_name, call_params) -> 
          (* TODO: Improve: Do parameter subsitution*)
          let stack_list = List.map (fun (name, _) -> name) stack in
          Format.fprintf debug_fmt "CALL: name:%s stack:%s\n" call_name (String.concat " " stack_list);
          if (Option.is_some (List.find_opt (fun (name, _) -> call_name = name) stack)) then
            raise (Fail "Recursive call")
          else (
            let MiGo_Types.Def(call_name, call_params, call_stmts) = Hashtbl.find def_hashtbl call_name in
            Format.sprintf "%s"
              (do_migo_to_ccs ((call_name, call_stmts)::(fun_name, stmt_tl)::tl))
          )
        | _ -> (do_migo_to_ccs ((fun_name, stmt_tl)::tl))
  in
    do_migo_to_ccs [("maim.main", stmts)]

