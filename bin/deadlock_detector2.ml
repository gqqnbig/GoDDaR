open Format
open Dlock
open Dlock.Auxfunctions
open Dlock.Main_act_verifier
open Dlock.Types
open Dlock.Types.Eta
open Dlock.Printer
open Dlock.List2
open Dlock.Cmd


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 

type possible_execution =
  (lambda  list) * (* List of the parallel compositions that can't reduce without syncronization*)
  (lambda  list) * (* List of the parallel compositions that might reduce wihtout syncronization,
                      and as such need to be processed before the previous list*)
  print_ctx

let eval_without_sync ((lambdas_sync, lambdas_no_sync, print_ctx) as execution: possible_execution) =
  match lambdas_no_sync with
  | [] -> [execution]
  | lambda::tl -> 
    match lambda with
    | LNil -> [(lambdas_sync, tl, print_ctx)]
    | LList(_, _) -> [(lambda::lambdas_sync, tl, print_ctx)]
    | LOr(a, b) -> [(lambdas_sync, a::tl, conc_lvl print_ctx "1"); (lambdas_sync, b::tl, conc_lvl print_ctx "2")]
    | LPar(a, b) -> [(lambdas_sync, a::b::tl, print_ctx)]
    | LSubst | LChi(_, _) -> failwith "These shouldn't appear"


let find_sync (top_environment: eta list) : (int * int list) list (* pairs (input index, output index list) list *) =
  let rec do_find_sync top_environment_remaining input_index top_environment result =
    match top_environment_remaining with
    | [] -> result
    | EEta(AIn(c))::tl ->
      let out_eta_indexes = findi ((=) (EEta(AOut(c)))) top_environment in
      let output_indexes = List.map (fun output_index -> output_index) out_eta_indexes in
      (do_find_sync [@tailcall]) tl (input_index+1) top_environment ((input_index, output_indexes)::result)
    | EEta(AOut(_))::tl -> 
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

let eval_sync ((lambdas_sync, lambdas_no_sync, print_ctx): possible_execution) =
  assert (List.length lambdas_no_sync = 0);
  let top_environment = List.map (
    function
    | LList(eta, _) -> eta
    | _ -> failwith "All lambda in Lambda_sync must start with LList"
  ) lambdas_sync in
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

let eval (lambda: lambda) = 
  let rec do_eval (executions: possible_execution list) (deadlocks: possible_execution list)=
    match executions with
    | [] -> deadlocks
    | ((lambdas_sync, lambdas_no_sync, print_ctx) as execution)::tl -> 
      if (List.length lambdas_no_sync) <> 0 then
        (do_eval [@tailcall]) ((eval_without_sync execution)@tl) deadlocks
      else (
        printCtxLevel print_ctx;
        printMode fmt (assocLeftList lambdas_sync) print_ctx.print;
        let sync_possible_executions = eval_sync execution in
        if (List.length sync_possible_executions) = 0 then
          (do_eval [@tailcall]) tl (execution::deadlocks)
        else
          (do_eval [@tailcall]) (sync_possible_executions@tl) deadlocks
      )
  in
    do_eval [([], [lambda], {level="1"; print=true})] []

let main exp =
  try
    Printexc.record_backtrace true;
    let lamExp = toLambda exp in
    (* Process Completeness Verification *)
    let act_ver = main_act_verifier lamExp in
    if has_miss_acts act_ver then
      (printMode fmt lamExp true; printf "\n"; print_act_ver act_ver)
    else (
      let deadlocked_executions = (eval lamExp) in
      printf "\n";
      List.iter (
        fun ((lambdas_sync, lambdas_no_sync, print_ctx): possible_execution) (* deadlock *) ->
          assert (List.length lambdas_no_sync = 0);
          if List.length lambdas_sync <> 0 then
            printMode fmt (assocLeftList lambdas_sync) print_ctx.print;
      ) deadlocked_executions
    )
  with
  | _ -> Printexc.print_backtrace stdout
;;

if (!process = "") then (
  main (CCS.parse "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0")
) else
  main (CCS.parse !process)