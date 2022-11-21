open Format
open Auxfunctions
open Main_act_verifier
open Types
open Printer
open List2
open Cmd


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

type state =
  (lambda_tagged list) *     (* List of the parallel compositions *)
  print_ctx

type prev_state = (eta LambdaC.lambdaC list * print_ctx)
  

let stateToLambda (lambdas, print_ctx: state): lambda =
  assocLeftList (List.map lambdaTaggedToLambda lambdas)

let print_state fmt (lambdas, print_ctx: state) = 
  printCtxLevel_noln fmt print_ctx;
  Format.fprintf fmt "    ";
  printMode fmt (assocLeftList (List.map lambdaTaggedToLambda (lambdas))) print_ctx.print;
  flush stdout

type sync_mode =
  | NoSync
  | MustSync
  | Sync of action

let eval_sync ((lambdas, print_ctx) as state: state): state list =
  let rec do_eval_sync (action: sync_mode) ((lambdas, print_ctx): state): state list =
    match action, lambdas with
    | _, [] -> []
    | NoSync  , (LList(EEtaTagged(a, _), b) as l)::tl
    | MustSync, (LList(EEtaTagged(a, _), b) as l)::tl ->
      (
        if action = NoSync then 
          (do_eval_sync NoSync (tl, print_ctx))
          |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
        else []
      ) @ (
        (do_eval_sync (Sync(a)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (b::lambdas, print_ctx))
      )
    | NoSync  , (LRepl(EEtaTagged(a, _), b) as l)::tl
    | MustSync, (LRepl(EEtaTagged(a, _), b) as l)::tl ->
      (
        if action = NoSync then 
          (do_eval_sync NoSync (tl, print_ctx))
          |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
        else []
      ) @ (
        (do_eval_sync (Sync(a)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (b::l::lambdas, print_ctx))
      )
    | Sync(action), (LList(EEtaTagged(a, _), b) as l)::tl ->
      if a = compl_action action then (
        (* found match *)
        [(b::tl, {print_ctx with level = print_ctx.level ^ "." ^ (actionToString a)})]
        @
        (do_eval_sync (Sync(action)) (tl, print_ctx))
      ) else (
        (* Keep seaching*)
        (do_eval_sync (Sync(action)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
      )
    | Sync(action), (LRepl(EEtaTagged(a, _), b) as l)::tl ->
      if a = compl_action action then (
        (* found match *)
        [(b::l::tl, {print_ctx with level = print_ctx.level ^ "." ^ (actionToString a)})]
      ) else (
        (* Keep seaching*)
        (do_eval_sync (Sync(action)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
      )
    | NoSync, LOrI(a, b)::tl ->
      [(a::tl, conc_lvl print_ctx "+1"); (b::tl, conc_lvl print_ctx "+2")]
    | Sync(_) , LOrI(a, b)::tl
    | MustSync, LOrI(a, b)::tl -> 
      let res1 = (do_eval_sync action (a::tl, conc_lvl print_ctx "+1")) in
      let res2 = (do_eval_sync action (b::tl, conc_lvl print_ctx "+2")) in
      res1 @ res2
    | NoSync  , LOrE(a, b)::tl
    | MustSync, LOrE(a, b)::tl ->
      (
        if action = NoSync then (
          (do_eval_sync action ([a], conc_lvl print_ctx "&1"))
          |> List.map (fun (lambdas, print_ctx) -> (LOrE((assocLeftList lambdas), b)::tl, print_ctx))
        ) @ (
          (do_eval_sync action ([b], conc_lvl print_ctx "&2")) 
          |> List.map (fun (lambdas, print_ctx) -> (LOrE(a, (assocLeftList lambdas))::tl, print_ctx))
        ) else []
      ) @ (
        do_eval_sync MustSync (a::tl, print_ctx)
      ) @ (
        do_eval_sync MustSync (b::tl, print_ctx)
      )
    | Sync(c), LOrE(a, b)::tl ->
      (
        do_eval_sync (Sync(c)) (a::tl, conc_lvl print_ctx "&1")
      ) @ (
        do_eval_sync (Sync(c)) (b::tl, conc_lvl print_ctx "&2")
      )
    | _, LPar(a, b)::tl ->
      do_eval_sync action (a::b::tl, print_ctx)
    | _, LNil::tl ->
      do_eval_sync action (tl, print_ctx)
  in
  (do_eval_sync NoSync (state))
  |> List.map (
    fun ((lambdas, print_ctx)) ->
      (List.filter ((<>) LNil) lambdas, print_ctx)
  )

let is_LNil_or_LRepl l =
  match l with
  | LNil | LRepl(_, _) -> true
  | _ -> false

let rec prev_state_contained_in_state ((ps_lambdas, ctx): prev_state) (lambdas: eta_tagged LambdaC.lambdaC list) =
  let rec inner (ps_l: eta LambdaC.lambdaC) (lambdas: eta_tagged LambdaC.lambdaC list) =
      match lambdas with
      | [] -> raise (RuntimeException "asd")
      | l_hd::l_tl -> 
        if (LambdaC.eta_equals_eta_tagged ps_l l_hd) then
          l_tl
        else
          l_hd::(inner ps_l l_tl)
  in
  match ps_lambdas with
  | [] -> lambdas
  | ps_hd::ps_tl ->
    let l = inner ps_hd lambdas in
      prev_state_contained_in_state (ps_tl, ctx) l

let find_duplicates (lambdas: eta_tagged LambdaC.lambdaC list) (prev_states: prev_state list):
  (lambda_tagged list * (lambda list * print_ctx)) list =
  prev_states
  |> List.filter_map (
    fun (((ps, ps_ctx) as prev_state): prev_state): 'a option ->
      try 
        let remaining = prev_state_contained_in_state prev_state lambdas in
        Some( (remaining, ( ps, ps_ctx)) )
      with
      | _ -> None
  )
  |> List.map (fun (l1, (l2, ctx)) -> (List.map LambdaC.lambdaCToLambda l1, ((List.map LambdaC.lambdaCToLambda l2), ctx)))

let eval fmt (lambda: lambda_tagged) = 
  let rec do_eval (states: (state * prev_state list) list) (deadlocks: state list) =
    match states with
    | [] -> List.rev deadlocks
    | (((lambdas, print_ctx) as state), prev_states)::tl -> 
      print_state fmt state;
      (* Strip LNil processes *)
      let lambdas = List.map (remLNils) lambdas in
      if (List.for_all is_LNil_or_LRepl lambdas) then
        do_eval tl deadlocks
      else (
        let reductions = eval_sync state in
        if reductions = [] then
          do_eval tl (state::deadlocks)
        else (
          let lambdasC =
            lambdas
            |> List.map lparToList
            |> List.flatten
            |> List.map remLNils 
            |> List.map lambdaTaggedToLambda
            |> List.map LambdaC.lambdaToLambdaC
          in
          let reductions = reductions
          |> List.map (fun r -> (r, (lambdasC, print_ctx)::prev_states))
          |> List.map (
            fun (((lambdas, ctx) as state, prev_states): (state * prev_state list) ): (state * prev_state list) list -> 
              let lambdasC =
                lambdas
                |> List.map lparToList
                |> List.flatten
                |> List.map remLNils 
                |> List.map LambdaC.lambdaToLambdaC
              in
              let dupl = find_duplicates lambdasC prev_states in
              if dupl = [] then (
                [((lambdas, ctx), prev_states)]
              ) else (
                print_state fmt state;
                Format.fprintf fmt "    DUPLICATES: \n";
                List.map (
                  fun (remaining, (common, common_ctx)) ->
                    Format.fprintf fmt "    ";
                    printMode_no_nl fmt (lambdaTaggedToLambda (assocLeftList remaining)) true;
                    Format.fprintf fmt " ; ";
                    printMode_no_nl fmt (assocLeftList common) true;
                    Format.fprintf fmt " -- %s\n" common_ctx.level;

                    ((remaining, ctx), prev_states)
                ) dupl
              )
          ) 
          |> List.flatten
        in
          do_eval (reductions@tl) deadlocks
        )
      )
  in
    do_eval [(([lambda], {level="1"; print=true}), [])] []

let rec deadlock_solver_1 (lambda: lambda_tagged) (deadlocked_top_environment: eta_tagged list): (lambda_tagged) =
  match lambda with
  (* If eta is prefixed with LNil, then theres no need to parallelize *)
  | LList(eta, LNil) -> LList(eta, LNil)
  | LList(eta, l) when List.mem eta deadlocked_top_environment -> LPar(LList(eta, LNil), deadlock_solver_1 l deadlocked_top_environment)
  | LList(eta, l) -> LList(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LRepl(eta, l) -> LRepl(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LPar(a, b) -> LPar(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrI(a, b) -> LOrI(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrE(a, b) -> LOrE(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LNil -> LNil

let deadlock_solver_2_dfs (lambda: lambda_tagged) (deadlocked_top_environment: eta_tagged list): (lambda_tagged) =
  let dte = ref (List.map (fun eta -> (eta, false)) deadlocked_top_environment) in
  let rec do_deadlock_solver_2 (lambda: lambda_tagged): (lambda_tagged) =
    let is_co_input c1 (eta, found_co_output) =
      match (eta, found_co_output) with
      | (EEtaTagged(AOut(_),_), _) -> false
      | (EEtaTagged(AIn(c2),_), _) -> c1 = c2 && (not found_co_output)
    in
    match lambda with
    | LList((EEtaTagged(AOut(_), _) as eta), l) when List.mem_assoc eta !dte ->
      dte := List.filter ((<>) (eta, false)) !dte;
      LPar(LList(eta, LNil), do_deadlock_solver_2 l)

    | LList( EEtaTagged(AOut(c1), _)       , l) when List.exists (is_co_input c1) !dte ->
      let (eta_input, _) = List.find (is_co_input c1) !dte in
      dte := (List.remove_assoc eta_input !dte);
      dte := (eta_input, true)::!dte;
      do_deadlock_solver_2 l

    | LList((EEtaTagged(AIn(c), tag) as eta), l) when List.mem_assoc eta !dte ->
      LPar(LList(EEtaTagged(AOut(c), tag), LNil), LList(eta, do_deadlock_solver_2 l))

    | LList(eta, l) -> LList(eta, do_deadlock_solver_2 l)
    | LRepl(eta, l) -> LRepl(eta, do_deadlock_solver_2 l)
    | LPar(a, b) -> LPar(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LOrI(a, b) -> LOrI(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LOrE(a, b) -> LOrE(do_deadlock_solver_2 a, do_deadlock_solver_2 b)
    | LNil -> LNil
  in
    do_deadlock_solver_2 lambda

let deadlock_solver_2 = deadlock_solver_2_dfs

let rec top_environment ((lambdas, print_ctx): state): eta_tagged list =
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
let rec detect_and_resolve fmt lambdaTaggedExp =
  (* Format.printf "DaR: \n";
  lambdaTaggedToLambda lambdaTaggedExp
  |> toProc
  |> print_lambda_simple Format.std_formatter; *)
  let deadlocked_states = (eval fmt lambdaTaggedExp) in
  Format.fprintf fmt "\n";
  if deadlocked_states = [] then (
    (true, [], [lambdaTaggedExp])
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
    let solved_exp = (remLNils (deadlock_solver lambdaTaggedExp deadlocked_top_environments)) in

    (true, deadlocked_states, [solved_exp])
  )


let main fmt exp: bool * lambda list * lambda list (*passed act_ver * deadlocked processes * resolved process*) =
  try
    Printexc.record_backtrace true;
    (* Process Completeness Verification *)
    let act_ver = main_act_verifier exp in
    if false then (
      printMode fmt exp true;
      fprintf fmt "\n";
      print_act_ver fmt act_ver;
      (false, [], [])
    ) else (
      let lambdaTaggedExp = lambdaToLambdaTagged exp in
      (* Ideally, we would just loop until no dealdock is found and discard the intermediary results.
         But the original implementation returns the first set of deadlocks and the fully deadlock
         resolved expression, so here we do the same. *)
      let (passed_act_ver, deadlocks, resolved) = detect_and_resolve fmt lambdaTaggedExp in

      if deadlocks = [] then (
        fprintf fmt "\nNo deadlocks!\n";
      ) else (
        fprintf fmt "\nDeadlocks:\n";
        List.iter (print_state fmt) deadlocks;
      );

      let rec detect_and_resolve_loop (passed_act_ver, deadlocked, resolved) (last_resolved: lambda_tagged list option)= 
        match last_resolved with
        (* When resolved program remains the same then exit loop*)
        | Some(last_resolved) when List.equal (=) last_resolved resolved ->
          (passed_act_ver, deadlocked, List.map lambdaTaggedToLambda resolved)
        (* When no deadlocks are found then exit loop*)
        | _ when deadlocked = [] -> 
          (passed_act_ver, deadlocked, List.map lambdaTaggedToLambda resolved)
        | _ -> 
          let res = detect_and_resolve null_fmt (List.hd resolved) in
          detect_and_resolve_loop res (Some(resolved))
      in
      let (_, _, resolved) = detect_and_resolve_loop (passed_act_ver, deadlocks, resolved) None in

      if deadlocks <> [] then (
        fprintf fmt "Resolved: %b \n" (has_miss_acts act_ver);
        printMode fmt (List.hd resolved) true
      );
      (passed_act_ver, List.map stateToLambda deadlocks, resolved)
    )
  with
  | _ -> Printexc.print_backtrace stdout; exit 1