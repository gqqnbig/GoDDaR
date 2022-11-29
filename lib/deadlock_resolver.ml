open Types
open Cmd
open Format

let rec deadlock_solver_1 (go_fixer_fmt: formatter option) (lambda: LambdaTagged.t) (deadlocked_top_environment: EtaTagged.eta list): (LambdaTagged.t) =
  let deadlock_solver_1 = deadlock_solver_1 go_fixer_fmt in
  match lambda with
  (* If eta is prefixed with LNil, then theres no need to parallelize *)
  | LList(eta, LNil) -> LList(eta, LNil)
  | LList(EEta(_, t) as eta, l) when List.mem eta deadlocked_top_environment->
    Option.iter (fun fmt -> Format.fprintf fmt "PARALLELIZE\n%s\n" t;) go_fixer_fmt;
    LPar(LList(eta, LNil), deadlock_solver_1 l deadlocked_top_environment)
  | LList(eta, l) -> LList(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LRepl(eta, l) -> LRepl(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LPar(a, b) -> LPar(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrI(a, b) -> LOrI(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrE(a, b) -> LOrE(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LNil -> LNil

let deadlock_solver_2_dfs (go_fixer_fmt: formatter option) (lambda: LambdaTagged.t) (deadlocked_top_environment: EtaTagged.eta list): (LambdaTagged.t) =
  let dte = ref (List.map (fun eta -> (eta, None)) deadlocked_top_environment) in

  let rec do_deadlock_solver_2  (go_fixer_fmt: formatter option) (lambda: LambdaTagged.t): (LambdaTagged.t) =
    let do_deadlock_solver_2 = do_deadlock_solver_2 go_fixer_fmt in
    let is_co_input c1 ((eta, found_co_output): (EtaTagged.eta * EtaTagged.eta option)) =
      match (eta, found_co_output) with
      | (EEta(AOut(_),_), _) -> false
      | (EEta(AIn(c2),_), _) -> c1 = c2 && (Option.is_none found_co_output)
    in
    match lambda with
    | LList((EEta(AOut(_), t) as eta), l) when List.mem_assoc eta !dte ->
      Option.iter (fun fmt -> Format.fprintf fmt "PARALLELIZE\n%s\n" t;) go_fixer_fmt;
      if (l = LNil) then (
        LList(eta, do_deadlock_solver_2 l)
      ) else (
        LPar(LList(eta, LNil), do_deadlock_solver_2 l)
      )
    | LList( EEta(AOut(c1), _) as eta, l) when List.exists (is_co_input c1) !dte ->
      let (eta_input, _) = List.find (is_co_input c1) !dte in
      dte := (List.remove_assoc eta_input !dte);
      dte := (eta_input, Some(eta))::!dte;
      do_deadlock_solver_2 l

    | LList((EEta(AIn(c), tag) as eta), l) when List.mem_assoc eta !dte ->
      LPar(LList(EEta(AOut(c), tag), LNil), LList(eta, do_deadlock_solver_2 l))

    | LList(eta, l) -> LList(eta, do_deadlock_solver_2 l)
    | LRepl(eta, l) -> LRepl(eta, do_deadlock_solver_2 l)
    | LPar(a, b) -> LPar(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LOrI(a, b) -> LOrI(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LOrE(a, b) -> LOrE(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LNil -> LNil
  in
    let res = do_deadlock_solver_2 go_fixer_fmt lambda in
    Option.iter (fun go_fixer_fmt ->
      List.iter (
            function 
            | EtaTagged.EEta(_, tag_input), Some(EtaTagged.EEta(_, tag_output)) -> 
              Format.fprintf go_fixer_fmt "MOVE\n%s\n%s\n" tag_output tag_input;
            | _, _ -> ()
      ) !dte
    ) go_fixer_fmt;
    res

let deadlock_solver_2 = deadlock_solver_2_dfs

let rec top_environment ((lambdas: LambdaTagged.t list), print_ctx): EtaTagged.eta list =
  match lambdas with
  | [] -> []
  | LList(eta, _)::tl
  | LRepl(eta, _)::tl -> (* TODO *)
    eta::(top_environment (tl, print_ctx))
  | LOrE(a, b)::tl
  | LOrI(a, b)::tl ->
    (top_environment ([a], print_ctx))@(top_environment ([b], print_ctx))@top_environment (tl, print_ctx)
  | LPar(a, b)::tl ->
    top_environment (a::b::tl, print_ctx)
  | LNil::tl ->
    top_environment (tl, print_ctx)

(* A single iteration of a deadlock detection and resolution *)
let rec detect_and_resolve fmt (go_fixer_fmt: formatter option) eval lambdaTaggedExp =
  (* Format.printf "DaR: \n";
  lambdaTaggedToLambda lambdaTaggedExp
  |> toProc
  |> print_lambda_simple Format.std_formatter; *)
  let deadlocked_states = (eval fmt lambdaTaggedExp) in
  Format.fprintf fmt "\n";
  if deadlocked_states = [] then (
    (true, [], lambdaTaggedExp)
  ) else (
    let deadlocked_top_environments =
      deadlocked_states
      |> List.map top_environment
      |> List.flatten
      (* Remove Duplicates *)
      |> List.fold_left ( fun tl hd -> (if List.mem hd tl then tl else hd :: tl)) []
    in
    (* List.iter (fun eta -> (print_eta_tagged fmt eta; fprintf fmt "\n")) deadlocked_top_environments; *)
    let deadlock_solver = if !ds < 2 then deadlock_solver_1 else deadlock_solver_2 in
    let solved_exp = (LambdaTagged.remLNils (deadlock_solver go_fixer_fmt lambdaTaggedExp deadlocked_top_environments)) in

    (true, deadlocked_states, solved_exp)
  )

let rec detect_and_resolve_loop (go_fixer_fmt: formatter option) eval (passed_act_ver, deadlocked, resolved) (last_resolved: LambdaTagged.t list)= 
  if !go then (
    (* In go or ds=2 mode just loop once *)
    (* TODO: fix looping when ds=2 *)
    (passed_act_ver, deadlocked, resolved)
  ) else (
    match last_resolved with
    | [] when deadlocked = [] -> 
      (passed_act_ver, deadlocked, resolved)
    (* When resolved program remains the same then exit loop*)
    | _ when List.mem resolved last_resolved ->
      (passed_act_ver, deadlocked, resolved)
    (* When no deadlocks are found then exit loop*)
    | _ -> 
      let res = detect_and_resolve null_fmt go_fixer_fmt eval resolved in
      detect_and_resolve_loop go_fixer_fmt eval res (resolved::last_resolved)
  )