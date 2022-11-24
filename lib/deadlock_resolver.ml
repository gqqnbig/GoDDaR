open Types
open Cmd

let rec deadlock_solver_1 (lambda: LambdaTagged.t) (deadlocked_top_environment: EtaTagged.eta list): (LambdaTagged.t) =
  match lambda with
  (* If eta is prefixed with LNil, then theres no need to parallelize *)
  | LList(eta, LNil) -> LList(eta, LNil)
  | LList(EEta(_, t) as eta, l) when List.mem eta deadlocked_top_environment ->
    if !go then ( Format.printf "PARALLELIZE\n%s\n" t;);
    LPar(LList(eta, LNil), deadlock_solver_1 l deadlocked_top_environment)
  | LList(eta, l) -> LList(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LRepl(eta, l) -> LRepl(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LPar(a, b) -> LPar(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrI(a, b) -> LOrI(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrE(a, b) -> LOrE(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LNil -> LNil

let deadlock_solver_2_dfs (lambda: LambdaTagged.t) (deadlocked_top_environment: EtaTagged.eta list): (LambdaTagged.t) =
  let dte = ref (List.map (fun eta -> (eta, None)) deadlocked_top_environment) in

  let rec do_deadlock_solver_2 (lambda: LambdaTagged.t): (LambdaTagged.t) =
    let is_co_input c1 ((eta, found_co_output): (EtaTagged.eta * EtaTagged.eta option)) =
      match (eta, found_co_output) with
      | (EEta(AOut(_),_), _) -> false
      | (EEta(AIn(c2),_), _) -> c1 = c2 && (Option.is_none found_co_output)
    in
    match lambda with
    | LList((EEta(AOut(_), t) as eta), l) when List.mem_assoc eta !dte ->
      if !go then Format.printf "PARALLELIZE\n%s\n" t;
      LPar(LList(eta, LNil), do_deadlock_solver_2 l)

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
    let res = do_deadlock_solver_2 lambda in
    if !go then List.iter (
      function 
      | EtaTagged.EEta(_, tag_input), Some(EtaTagged.EEta(_, tag_output)) -> 
        Format.printf "MOVE\n%s\n%s\n" tag_output tag_input;
      | _, _ -> ()
    ) !dte; 
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
let rec detect_and_resolve fmt eval lambdaTaggedExp =
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
    let solved_exp = (LambdaTagged.remLNils (deadlock_solver lambdaTaggedExp deadlocked_top_environments)) in

    (true, deadlocked_states, solved_exp)
  )

let rec detect_and_resolve_loop eval (passed_act_ver, deadlocked, resolved) (last_resolved: LambdaTagged.t option)= 
  if !go || !ds > 1 then (
    (* In go or ds=2 mode just loop once *)
    (* TODO: fix looping when ds=2 *)
    (passed_act_ver, deadlocked, resolved)
  ) else (
    match last_resolved with
    (* When resolved program remains the same then exit loop*)
    | Some(last_resolved) when last_resolved = resolved ->
      (passed_act_ver, deadlocked, resolved)
    (* When no deadlocks are found then exit loop*)
    | _ when deadlocked = [] -> 
      (passed_act_ver, deadlocked, resolved)
    | _ -> 
      let res = detect_and_resolve null_fmt eval resolved in
      detect_and_resolve_loop eval res (Some(resolved))
  )