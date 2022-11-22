open Types

exception Fail of string

type stack_entry = string * (MiGo_Types.migo_stmt list) * (string * string) list

let debug_fmt = null_fmt

let check_has_been_called call_name stack =
  if (Option.is_some (List.find_opt (fun (name, _, _) -> call_name = name) stack)) then
    raise (Fail (Format.sprintf "Recursive call (%s)" call_name))

let rec rename_param c stack = 
  match stack with
  | (_, _, var_map )::tl -> (
      let new_c = List.assoc_opt c var_map in
      match new_c with
      | Some(c) -> rename_param c tl
      | None -> c
    )
  | [] -> c



let rec do_migo_to_ccs migo_defs (stack: stack_entry list): LambdaTagged.t =
  match stack with 
  | [] -> Format.fprintf debug_fmt "No more stack\n"; LNil
  | (fun_name, stmts, var_map)::tl ->
    match stmts with
    | [] -> Format.fprintf debug_fmt "No stmts\n"; do_migo_to_ccs migo_defs (tl)
    | stmt::stmt_tl ->
      match stmt with
      | MiGo_Types.Prefix(Send(c, tag)) ->
        LList(
          EEta(AOut(rename_param c stack), tag),
          (do_migo_to_ccs migo_defs ((fun_name, stmt_tl, var_map)::tl))
        )
      | MiGo_Types.Prefix(Receive(c, tag)) ->
        LList(
          EEta(AIn(rename_param c stack), tag),
          (do_migo_to_ccs migo_defs ((fun_name, stmt_tl, var_map)::tl))
        )
      | MiGo_Types.If(t, f) -> 
        LOrI(
          (do_migo_to_ccs migo_defs ((fun_name, t@stmt_tl, var_map)::tl)),
          (do_migo_to_ccs migo_defs ((fun_name, f@stmt_tl, var_map)::tl))
        )
      | MiGo_Types.Call(call_name, call_params) -> 
        check_has_been_called call_name stack;
        let MiGo_Types.Def(def_name, def_params, def_stmts) = Hashtbl.find migo_defs call_name in
        let call_var_map = List.combine def_params call_params in
          do_migo_to_ccs migo_defs ((def_name, def_stmts, call_var_map)::(fun_name, stmt_tl, var_map)::tl)
      | MiGo_Types.Spawn(call_name, call_params) -> 
        check_has_been_called call_name stack;
        let MiGo_Types.Def(def_name, def_params, def_stmts) = Hashtbl.find migo_defs call_name in
        let stack_without_stmts = List.map (fun (fun_name, stmts, var_map) -> (fun_name, [], var_map)) stack in
        let call_var_map = List.combine def_params call_params in
        LPar(
          (do_migo_to_ccs migo_defs ((fun_name, stmt_tl, var_map)::tl)),
          (do_migo_to_ccs migo_defs ((def_name, def_stmts, call_var_map)::stack_without_stmts))
        )
      | MiGo_Types.Select(cases) -> 
        let (tau_cases, other_cases) = List.partition (fun (prefix, _) -> prefix = MiGo_Types.Tau) cases in
        let gen_case = List.map (
          fun (prefix, stmts) -> 
            do_migo_to_ccs migo_defs ((fun_name, Prefix(prefix)::stmts, var_map)::tl)
        ) in
        let tau_cases = gen_case tau_cases in
        let other_cases = gen_case other_cases in
        if tau_cases = [] then (
          LambdaTagged.assocLOrEList other_cases
        ) else (
          LambdaTagged.assocLOrIList ((LambdaTagged.assocLOrEList other_cases)::tau_cases)
        )
        (* Format.sprintf "(%s)" (String.concat " & " other_case_strings)
        |> fun other_case_expr -> 
        if tau_case_strings = [] then (
          other_case_expr
        ) else (
          Format.sprintf "(%s)" (String.concat " + " (other_case_expr::tau_case_strings))
        ) *)
      | MiGo_Types.Lock(_)
      | MiGo_Types.Unlock(_)
      | MiGo_Types.Prefix(Tau)
      | MiGo_Types.Close(_)
      | MiGo_Types.Newchan(_) (* TODO: Check if channel names are unique *) -> 
        do_migo_to_ccs migo_defs ((fun_name, stmt_tl, var_map)::tl)

let migo_to_ccs (migo_defs: MiGo_Types.migo_def list): LambdaTagged.t = 
  let migo_def_hashtbl = Hashtbl.create (List.length migo_defs) in
  List.iter (
    fun (MiGo_Types.Def(name, params, stmts) as def) -> (
      Hashtbl.add migo_def_hashtbl name def
    )
  ) migo_defs;
  let Def(_, params, stmts) = (Hashtbl.find migo_def_hashtbl "main.main") in
  assert (params = []);
  do_migo_to_ccs migo_def_hashtbl [("main.main", stmts, [])]

