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
let get_top_layer (lambdas: LambdaTagged.t list) : LambdaTagged.t list = 
  let rec do_get_top_layer (lambda: LambdaTagged.t) : LambdaTagged.t list =
    match lambda with
    | LList(eta, _)
    | LRepl(eta, _) as l ->
      [l]
    | LOrE(a, b)
    | LOrI(a, b)
    | LPar(a, b) ->
      (do_get_top_layer a)@(do_get_top_layer b)
    | LNil ->
      []
  in
  lambdas |>
  List.map do_get_top_layer
  |> List.flatten

let get_next_layer (lambdas: LambdaTagged.t list) : LambdaTagged.t list =
  let rec do_get_next_layer (lambda: LambdaTagged.t) : LambdaTagged.t list =
    match lambda with
    | LList(eta, l)
    | LRepl(eta, l) ->
      get_top_layer [l]
    | LOrE(a, b)
    | LOrI(a, b)
    | LPar(a, b) ->
      (do_get_next_layer a)@(do_get_next_layer b)
    | LNil ->
      []
  in
  lambdas |>
  List.map do_get_next_layer
  |> List.flatten

let get_top_eta lambda =
  match lambda with
  | LambdaTagged.LList(eta, _)
  | LRepl(eta, _) -> eta
  | _ -> failwith "This should not happen!"

let get_Etas_by_layer lambda: (Eta.eta, int) Hashtbl.t = 
  let etas_by_layer = Hashtbl.create (0) in
  let add_or_replace_min (eta: Eta.eta) (layer: int) =
    match Hashtbl.find_opt etas_by_layer eta with
    | Some(old_layer) -> if layer < old_layer then Hashtbl.replace etas_by_layer eta layer
    | None -> Hashtbl.add etas_by_layer eta layer
  in
  let rec iterate_layers lambda layer =
    if lambda <> [] then (
      List.iter (
        fun lambda ->
          add_or_replace_min (etaTaggedToEta (get_top_eta lambda)) layer
      ) lambda;
      let new_lambda = get_next_layer lambda in
      iterate_layers new_lambda (layer+1)
    )
  in
  iterate_layers (get_top_layer lambda) 0;
  etas_by_layer

(* Receives a deadlocked state.
   For each problematic action return a score (heuristic indicating how good a choice that ) *)
let get_blocked_eta_scores ((lambdas, ctx): state): (EtaTagged.eta * int) list = 
  (* Hashmap of eta to layer number*)
  let etas_by_layer = get_Etas_by_layer lambdas in

  Format.fprintf debug_fmt "etas_by_layer:\n";
  Hashtbl.iter (
    fun eta layer -> Format.fprintf debug_fmt "- %a: %i\n" Eta.print_eta eta layer) etas_by_layer;

  get_top_layer lambdas
  |> List.filter_map (
    fun lambda -> (
      let blocked_eta = get_top_eta lambda in
      let compl_eta_scores: (Eta.eta * int) list =
        get_next_layer [lambda]
        (* List of compl etas of the etas directly bellow the blocked eta *)
        |> List.map (fun l -> l |> get_top_eta |> EtaTagged.compl_eta)
        (* make a pair of the compl_eta and the layer number where the compl_eta is located *)
        |> List.filter_map (
          fun compl_eta ->
            Hashtbl.find_opt etas_by_layer compl_eta
            |> Option.map ( fun (layer) -> (compl_eta, layer))
        )
      in

      (* Debug print *)
      List.iter (
        fun (compl_eta, layer) ->
          Format.fprintf debug_fmt "blocked: %a; compl_eta: %a; layer: %i\n"
            EtaTagged.print_eta_simple blocked_eta
            Eta.print_eta_simple compl_eta
            layer
      ) compl_eta_scores;

      let blocked_eta_score = 
        compl_eta_scores
        |> List.map ( fun (compl_eta, layer) -> layer)
        (* Get the eta *)
        |> List.fold_left (min) max_int
      in
      if blocked_eta_score = max_int then
        None
      else 
        Some(blocked_eta, blocked_eta_score)
    )
  )

(* A single iteration of a deadlock detection and resolution *)
let rec detect_and_resolve fmt (go_fixer_fmt: formatter option) eval lambdaTaggedExp =
  Format.fprintf debug_fmt "DaR: %a\n" LambdaTagged.print lambdaTaggedExp;
  let deadlocked_states = (eval fmt lambdaTaggedExp) in
  Format.fprintf fmt "\n";
  let best_eta_layer =
    deadlocked_states
    |> List.map get_blocked_eta_scores
    |> List.flatten
    |> List.map Option.some
    |> List.fold_left (
      fun p1 p2 -> (
        match p1, p2 with
        | Some(eta1, score1), Some (eta2, score2) ->
          if score1 < score2 then Some(eta1, score1) else Some(eta2, score2)
        | None, None -> None
        | None, Some _ -> p2
        | Some _, None -> p1
      )) None
  in
  match best_eta_layer with
  | None ->
    (true, deadlocked_states, lambdaTaggedExp)
  | Some(best_eta, _) ->  (
    let deadlock_solver = if !ds < 2 then deadlock_solver_1 else deadlock_solver_2 in
    let solved_exp = (LambdaTagged.remLNils (deadlock_solver go_fixer_fmt lambdaTaggedExp [best_eta])) in

    (true, deadlocked_states, solved_exp)
  )

let rec detect_and_resolve_loop (go_fixer_fmt: formatter option) eval (passed_act_ver, deadlocked, resolved) (last_resolved: LambdaTagged.t list)= 
  if !go then (
    (* In go mode just loop once *)
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