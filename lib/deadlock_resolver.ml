open Types
open Cmd
open Format
open Deadlock_resolver_heuristics

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


(* A single iteration of a deadlock detection and resolution *)
let rec detect_and_resolve fmt (go_fixer_fmt: formatter option) eval exp (deps: dependency list) =
  let deadlock_solver = if !ds < 2 then deadlock_solver_1 else deadlock_solver_2 in

  Format.fprintf debug_fmt "DaR: %a\n" LambdaTagged.print exp;
  let deadlocked_states = (eval fmt exp) in
  Format.fprintf fmt "\n";

  let all_deps =
    deps
    |> List.map (fun (a, b) -> a::b)
    |> List.flatten
  in

  let best_eta = (
    if !all_etas then (
      let best_etas = Heuristic_NOP.best_etas exp deadlocked_states in
      Heuristic_NOP.print_eta_score debug_fmt best_etas;
      best_etas
      |> List.map (fun (eta, _) -> eta)
    ) else (
      (* let best_etas = Heuristic_by_layer.best_etas exp deadlocked_states in
      Heuristic_by_layer.print_eta_score debug_fmt best_etas;
      Heuristic_by_layer.best_eta exp best_etas *)
      let best_etas = Heuristic_New.best_etas exp deadlocked_states in
      Heuristic_New.print_eta_score debug_fmt best_etas;
      best_etas
      (* Find the eta that doesn't have a dependency*)
      |> List.find_map (
        fun (EtaTagged.EEta(_, t) as eta, _) ->(
          if (List.mem t all_deps) then
            None
          else
            Some(eta)
        )
      ) 
      (* |> List.find_map (fun (eta, _) -> Some(eta)) *)
      |> Option.to_list
    )
  ) in
  match best_eta with
  | [] ->
    (deadlocked_states, exp)
  | _ ->  (
    let solved_exp = (LambdaTagged.remLNils (deadlock_solver go_fixer_fmt exp best_eta)) in
    (deadlocked_states, solved_exp)
  )



let rec detect_and_resolve_loop (go_fixer_fmt: formatter option) eval (deadlocked, resolved) deps (last_resolved: LambdaTagged.t list)= 
  (
    match last_resolved with
    | [] when deadlocked = [] -> 
      (true, deadlocked, resolved)
    (* When resolved program remains the same then exit loop*)
    | _ when List.mem resolved last_resolved ->
      (deadlocked = [], deadlocked, resolved)
    (* When no deadlocks are found then exit loop*)
    | _ -> 
      let res = detect_and_resolve null_fmt go_fixer_fmt eval resolved deps in
      detect_and_resolve_loop go_fixer_fmt eval res deps (resolved::last_resolved)
  )