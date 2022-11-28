open Types
open Cmd
open Format


(* This tupple store some data about a possible execution of the program
    ((eta list) list) stores the sequence of executed actions for parallel processes that have been fully processed

    (lambda * (eta list)) list, stores the list of parallel executions together with the
      respective sequence of executed actions that have been performed so far 

      At the end, the first list should contain all the possible action sequences for the parallel
      process for this possible execution, and the second list should be empty
   *)
type possible_execution = ( ((Eta.eta list) list) * (Lambda.t * (Eta.eta list)) list)

(* Receives a list of possible executions. Iterates on them to find all possible executions and
   for each possible execution found, returns the sequence of actions that each parallel process in
   the possible execution can execute 
   
   eta
   list (sequence of etas of the parallel process)
   list (of parallel processes in that possible execution)
   list (of possible executions)
   *)
let possible_executions exp =
  let rec do_possible_executions (possible_executions : possible_execution list) result =
    match possible_executions with
    | [] -> result
    | (eta_sequences, parallel_processes)::tl_possible_executions ->

      match parallel_processes with
      | [] -> do_possible_executions tl_possible_executions (eta_sequences::result)
      | (parallel_process, eta_sequence)::tl_parallel_processes -> 
        let possible_execution' =
          match parallel_process with
            (* Add eta to eta sequence*)
            | LList(eta, l)
            | LRepl(eta, l)
              -> [(eta_sequences, (l, eta::eta_sequence)::tl_parallel_processes)]
            (* Append the two possible exectutions *)
            | LOrI(a, b)
            | LOrE(a, b) -> (eta_sequences, (a, eta_sequence)::tl_parallel_processes)::
                            (eta_sequences, (b, eta_sequence)::tl_parallel_processes)::[]
            (* Add the extra parallel process  *)
            | LPar(a, b) -> [(eta_sequences, (a, eta_sequence)::(b, [])::tl_parallel_processes)]
            | LNil -> [((List.rev eta_sequence)::eta_sequences, tl_parallel_processes)]
        in
        do_possible_executions (possible_execution' @ tl_possible_executions) result
  in
  do_possible_executions [([], [(exp, [])])] []

let eta_to_chan_delta (eta: Eta.eta) = 
  match eta with
    | EEta(AIn(c)) -> (c, -1)
    | EEta(AOut(c)) -> (c, +1)

let chan_delta_to_eta (chan, delta): Eta.eta =
  if delta < 0 then
    EEta(AIn(chan))
  else
    EEta(AOut(chan))

(** Receives a lambda representation of a process

    Returns [(lambda * eta list) list]. Generates a list of lambdas, and for each one return a
    list of [eta]/actions with an unequal occurence counter compared to [compl_eta eta] *)
let main_act_verifier (exp: Lambda.t) : (Lambda.t * Eta.eta list) list=
  let res = possible_executions exp in
  (* For each possible_execution, transform the list of eta sequence to a tupple [(lambda * eta list)].
    [lambda] is the representation of the process execution and [eta list] the list of unbalenced actions.
     *)
  List.map (fun possible_execution -> 
    (* List of all actions found in this execution, between all the parallel processes*)
    let list_of_all_etas = List.flatten possible_execution in

    (* Create and populate an hash table mapping each channel with the respective "sum" of the actions performed.
      As implemented in [eta_to_chan_delta], an input is worth -1, output +1.
      If the sum of the actions equals 0, then the number of inputs equals the number of outputs. *)
    let channel_counter = Hashtbl.create (List.length list_of_all_etas) in
    List.iter (fun eta -> (
        let (c, delta) = eta_to_chan_delta eta in
        match (Hashtbl.find_opt channel_counter c) with
        | None -> Hashtbl.add channel_counter c delta
        | Some(counter) -> Hashtbl.replace channel_counter c (counter + delta)
      )
    ) list_of_all_etas;

    let list_of_channel_counter = List.of_seq (Hashtbl.to_seq channel_counter) in
    let unbalenced_actions = List.filter (fun (channel, counter) -> counter <> 0) list_of_channel_counter in

    (* Paralelize the [LList]s *)
    Lambda.assocLeftList (List.map (
      fun eta_list -> (
          (*[Eta list] to [LList]/[lambda]*)
          List.fold_right (fun eta lambda -> Lambda.LList(eta, lambda)) eta_list LNil
        )
      ) possible_execution
    ),
    List.map (fun pair -> chan_delta_to_eta pair) unbalenced_actions
  ) res


let has_miss_acts ( list : (Lambda.t * Eta.eta list) list ) =
  List.exists (fun (lambda, eta_list) -> (List.length eta_list) <> 0) list

let print_act_ver fmt (arr: (Lambda.t * Eta.eta list) list) =
  let rec print arr =
    match arr with
    | [] -> ()
    | hd::tl -> 
        match hd with
        | (a, []) -> print tl
        | (a, b) -> 
          if !verbose then (
            fprintf fmt "- %a in %a\n" Eta.print_etalist_alt b Lambda.print a;
            print tl
          ) else (
            fprintf fmt "- %a in %a" Eta.print_etalist_alt_simple b Lambda.print a;
            print tl
          )
  in
  if !verbose then
    fprintf fmt "Action(s) missing correspondence(s) in process(es):\n";
    print arr