open Format
open Auxfunctions
open Main_act_verifier
open Types
open Printer
open List2
open Cmd


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

type possible_execution =
  (lambda_tagged  list) * (* List of the parallel compositions that can't reduce without syncronization*)
  (lambda_tagged  list) * (* List of the parallel compositions that might reduce wihtout syncronization,
                      and as such need to be processed before the previous list*)
  print_ctx

let possible_executionToLambda (lambdas_sync, lambdas_no_sync, print_ctx: possible_execution): lambda =
  let lambdas_sync = List.map lambdaTaggedToLambda lambdas_sync in
  let lambdas_no_sync = List.map lambdaTaggedToLambda lambdas_no_sync in
  assocLeftList (lambdas_sync@lambdas_no_sync)

let print_possible_execution fmt (lambdas_sync, lambdas_no_sync, print_ctx: possible_execution) = 
  printCtxLevel_noln fmt print_ctx;
  printMode_no_nl fmt (assocLeftList (List.map lambdaTaggedToLambda (lambdas_no_sync@lambdas_sync))) print_ctx.print;
  flush stdout

let eval_without_sync fmt ((lambdas_sync, lambdas_no_sync, print_ctx) as execution: possible_execution) =
  match lambdas_no_sync with
  | [] -> [execution]
  | lambda::tl -> 
    match lambda with
    | LNil -> [(lambdas_sync, tl, print_ctx)]
    | LList(_, _) -> [(lambda::lambdas_sync, tl, print_ctx)]
    | LOrI(a, b) -> print_possible_execution fmt execution; [(lambdas_sync, a::tl, conc_lvl print_ctx "1"); (lambdas_sync, b::tl, conc_lvl print_ctx "2")]
    | LPar(a, b) -> [(lambdas_sync, a::b::tl, print_ctx)]
    | LSubst | LChi(_, _) -> failwith "These shouldn't appear"


let find_sync (top_environment: eta_tagged list) : (int * int list) list (* pairs (input index, output index list) list *) =
  let rec do_find_sync (top_environment_remaining: eta_tagged list) input_index (top_environment: eta_tagged list) result =
    match top_environment_remaining with
    | [] -> result
    | EEtaTagged((AIn(c) as a), tag)::tl ->
      let output_indexes = findi (fun (EEtaTagged(b, _): eta_tagged) -> a = compl_action b) top_environment in
      (do_find_sync [@tailcall]) tl (input_index+1) top_environment ((input_index, output_indexes)::result)
    | EEtaTagged(AOut(_), tag)::tl -> 
      (do_find_sync [@tailcall]) tl (input_index+1) top_environment result
  in
    do_find_sync top_environment 0 top_environment []

let sync lambdas_sync input_index output_index =
  let rec do_sync current_index lambdas_sync input_index output_index (updated_lambdas_sync, updated_lambdas_no_sync) =
    match lambdas_sync with
    | [] -> (updated_lambdas_sync, updated_lambdas_no_sync)
    | (LList(eta, l) as lambda)::tl ->
      if current_index = input_index || current_index = output_index then
        let updated_lambda = if l = LNil then [] else [l] in
        do_sync (current_index+1) tl input_index output_index (updated_lambdas_sync, updated_lambda@updated_lambdas_no_sync)
      else
        do_sync (current_index+1) tl input_index output_index (lambda::updated_lambdas_sync, updated_lambdas_no_sync)
    | _ -> failwith "All lambda in Lambda_sync must start with LList"
  in
  do_sync 0 lambdas_sync input_index output_index ([], [])

let top_environment ((lambdas_sync, lambdas_no_sync, print_ctx): possible_execution) =
  List.map (
    function
    | LList(eta, _) -> eta
    | _ -> failwith "All lambda in Lambda_sync must start with LList"
  ) lambdas_sync

let eval_sync ((lambdas_sync, lambdas_no_sync, print_ctx) as execution: possible_execution) =
  assert (List.length lambdas_no_sync = 0);
  let top_environment = top_environment execution in
  let sync_index_pair_list = find_sync top_environment in
  let i = ref 0 in
  List.flatten (
    (* For each input action *)
    List.map (
      fun (input_index, output_index_list) ->
        (* For each output synced with the input *)
        List.map (
          fun output_index -> (
            (* [lambdas_sync] without input and output action, effectivly simulating the syncronization *)
            let (updated_lambdas_sync, updated_lambdas_no_sync) = sync lambdas_sync input_index output_index in
            i := !i+1;
            ((updated_lambdas_sync, updated_lambdas_no_sync, conc_lvl print_ctx (string_of_int !i)): possible_execution)
          )
        ) output_index_list
    ) sync_index_pair_list
  )

let eval fmt (lambda: lambda_tagged) = 
  let rec do_eval (executions: possible_execution list) (deadlocks: possible_execution list)=
    match executions with
    | [] -> List.rev deadlocks
    | ((lambdas_sync, lambdas_no_sync, print_ctx) as execution)::tl -> 
      let (updated_possible_executions_list, updated_deadlocks) = 
        (* Reduce the process until only syncronization is possible *)
        if lambdas_no_sync <> [] then (
          (((eval_without_sync fmt execution)@tl), deadlocks)
        ) else (
          print_possible_execution fmt execution;
          let sync_possible_executions = eval_sync execution in
          if sync_possible_executions = [] then
            if not (lambdas_sync = [] && lambdas_no_sync = []) then
              (tl, (execution::deadlocks))
            else
              (tl, deadlocks)
          else
            ((sync_possible_executions@tl), deadlocks)
      ) in
        do_eval updated_possible_executions_list updated_deadlocks
  in
    do_eval [([], [lambda], {level="1"; print=true})] []

let rec deadlock_solver_1 (lambda: lambda_tagged) (deadlocked_top_environment: eta_tagged list): (lambda_tagged) =
  match lambda with
  (* If eta is prefixed with LNil, then theres no need to parallelize *)
  | LList(eta, LNil) -> LList(eta, LNil)
  | LList(eta, l) when List.mem eta deadlocked_top_environment -> LPar(LList(eta, LNil), deadlock_solver_1 l deadlocked_top_environment)
  | LList(eta, l) -> LList(eta, deadlock_solver_1 l deadlocked_top_environment)
  | LPar(a, b) -> LPar(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrI(a, b) -> LOrI(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LOrE(a, b) -> LOrE(deadlock_solver_1 a deadlocked_top_environment, deadlock_solver_1 b deadlocked_top_environment)
  | LNil -> LNil
  | LSubst | LChi(_, _) -> failwith "These shouldn't appear"

let rec deadlock_solver_2 (lambda: lambda_tagged) (deadlocked_top_environment: eta_tagged list): (lambda_tagged) =
  match lambda with
  | LList((EEtaTagged(AOut(c), tag) as eta), l) when List.mem (EEtaTagged(AOut(c), tag)) deadlocked_top_environment ->
      LPar(LList(eta, LNil), deadlock_solver_2 l deadlocked_top_environment)
  | LList( EEtaTagged(AOut(c), tag)        , l) when List.mem (EEtaTagged(AIn(c) , tag)) deadlocked_top_environment ->
      deadlock_solver_2 l deadlocked_top_environment
  | LList((EEtaTagged(AIn(c), tag) as eta), l) when List.mem (EEtaTagged(AIn(c), tag)) deadlocked_top_environment ->
      LPar(LList(EEtaTagged(AOut(c), tag), LNil), LList(eta, deadlock_solver_2 l deadlocked_top_environment))
  | LList(eta, l) -> LList(eta, deadlock_solver_2 l deadlocked_top_environment)
  | LPar(a, b) -> LPar(deadlock_solver_2 a deadlocked_top_environment, deadlock_solver_2 b deadlocked_top_environment)
  | LOrI(a, b) -> LOrI(deadlock_solver_2 a deadlocked_top_environment, deadlock_solver_2 b deadlocked_top_environment)
  | LOrE(a, b) -> LOrE(deadlock_solver_2 a deadlocked_top_environment, deadlock_solver_2 b deadlocked_top_environment)
  | LNil -> LNil
  | LSubst | LChi(_, _) -> failwith "These shouldn't appear"


(* A single iteration of a deadlock detection and resolution *)
let rec detect_and_resolve fmt lambdaTaggedExp =
  let deadlocked_executions = (eval fmt lambdaTaggedExp) in
  if deadlocked_executions = [] then (
    (true, [], [lambdaTaggedExp])
  ) else (
    let deadlocked_top_environments = List.flatten (List.map top_environment deadlocked_executions) in
    (* List.iter (fun eta -> (print_eta_tagged fmt eta; fprintf fmt "\n")) deadlocked_top_environments; *)
    let deadlock_solver = if !ds < 2 then deadlock_solver_1 else deadlock_solver_2 in
    let solved_exp = (deadlock_solver lambdaTaggedExp deadlocked_top_environments) in
    (true, deadlocked_executions, [solved_exp])
  )

let main fmt exp: bool * lambda list * lambda list (*passed act_ver * deadlocked processes * resolved process*)=
  try
    Printexc.record_backtrace true;
    let lamExp = toLambda exp in
    (* Process Completeness Verification *)
    let act_ver = main_act_verifier lamExp in
    if has_miss_acts act_ver then (
      printMode fmt lamExp true;
      fprintf fmt "\n";
      print_act_ver fmt act_ver;
      (false, [], [])
    ) else (
      let lambdaTaggedExp = lambdaToLambdaTagged lamExp in
      (* Ideally, we would just loop until no dealdock is found and discard the intermediary results.
         But the original implementation returns the first set of deadlocks and the fully deadlock
         resolved expression, so here we do the same. *)
      let (passed_act_ver, deadlocks, resolved) = detect_and_resolve fmt lambdaTaggedExp in

      let rec detect_and_resolve_loop (passed_act_ver, deadlocked, resolved) (last_resolved: lambda_tagged list option)= 
        match last_resolved with
        | Some(last_resolved) when List.equal (=) last_resolved resolved ->
          (passed_act_ver, deadlocked, List.map lambdaTaggedToLambda resolved)
        | _ when deadlocked = [] -> 
          (passed_act_ver, deadlocked, List.map lambdaTaggedToLambda resolved)
        | _ -> 
          let res = detect_and_resolve null_fmt (List.hd resolved) in
          detect_and_resolve_loop res (Some(resolved))
      in
      let (_, _, resolved) = detect_and_resolve_loop (passed_act_ver, deadlocks, resolved) None in

      if deadlocks = [] then (
        fprintf fmt "\nNo deadlocks!\n";
      ) else (
        fprintf fmt "\nDeadlocks:\n";
        List.iter (print_possible_execution fmt) deadlocks;
        fprintf fmt "Resolved:\n";
        printMode fmt (List.hd resolved) true
      );
      (passed_act_ver, List.map possible_executionToLambda deadlocks, resolved)
    )
  with
  | _ -> Printexc.print_backtrace stdout; exit 1