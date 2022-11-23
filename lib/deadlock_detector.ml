open Format
open Main_act_verifier
open Types
open Cmd
open Deadlock_resolver


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

type state =
  (LambdaTagged.t list) *     (* List of the parallel compositions *)
  print_ctx

type prev_state = (LambdaC.t list * print_ctx)
  

let stateToLambda (lambdas, print_ctx: state): Lambda.t =
  Lambda.assocLeftList (List.map lambdaTaggedToLambda lambdas)

let print_state fmt (lambdas, print_ctx: state) = 
  printCtxLevel_noln fmt print_ctx;
  Format.fprintf fmt "    ";
  LambdaTagged.printMode fmt (LambdaTagged.assocLeftList (lambdas)) print_ctx.print;
  flush stdout

type sync_mode =
  | NoSync
  | MustSync
  | Sync of action

let eval_sync ((lambdas, print_ctx) as state: state): state list =
  let rec do_eval_sync (action: sync_mode) ((lambdas, print_ctx): state): state list =
    match action, lambdas with
    | _, [] -> []
    | NoSync  , (LList(EEta(a, _), b) as l)::tl
    | MustSync, (LList(EEta(a, _), b) as l)::tl ->
      (
        if action = NoSync then 
          (do_eval_sync NoSync (tl, print_ctx))
          |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
        else []
      ) @ (
        (do_eval_sync (Sync(a)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (b::lambdas, print_ctx))
      )
    | NoSync  , (LRepl(EEta(a, _), b) as l)::tl
    | MustSync, (LRepl(EEta(a, _), b) as l)::tl ->
      (
        if action = NoSync then 
          (do_eval_sync NoSync (tl, print_ctx))
          |> List.map (fun (lambdas, print_ctx) -> (l::lambdas, print_ctx))
        else []
      ) @ (
        (do_eval_sync (Sync(a)) (tl, print_ctx))
        |> List.map (fun (lambdas, print_ctx) -> (b::l::lambdas, print_ctx))
      )
    | Sync(action), (LList(EEta(a, _), b) as l)::tl ->
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
    | Sync(action), (LRepl(EEta(a, _), b) as l)::tl ->
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
          |> List.map (fun (lambdas, print_ctx) -> (LambdaTagged.LOrE((LambdaTagged.assocLeftList lambdas), b)::tl, print_ctx))
        ) @ (
          (do_eval_sync action ([b], conc_lvl print_ctx "&2")) 
          |> List.map (fun (lambdas, print_ctx) -> (LambdaTagged.LOrE(a, (LambdaTagged.assocLeftList lambdas))::tl, print_ctx))
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
      (List.filter ((<>) LambdaTagged.LNil) lambdas, print_ctx)
  )

let is_LNil_or_LRepl (l: LambdaTagged.t) =
  match l with
  | LNil | LRepl(_, _) -> true
  | _ -> false

let rec prev_state_contained_in_state ((ps_lambdas, ctx): prev_state) (lambdas: LambdaCTagged.t list) =
  let rec inner (ps_l: LambdaC.t) (lambdas: LambdaCTagged.t list) =
      match lambdas with
      | [] -> raise (RuntimeException "asd")
      | l_hd::l_tl -> 
        if (eta_equals_eta_tagged ps_l l_hd) then
          l_tl
        else
          l_hd::(inner ps_l l_tl)
  in
  match ps_lambdas with
  | [] -> lambdas
  | ps_hd::ps_tl ->
    let l = inner ps_hd lambdas in
      prev_state_contained_in_state (ps_tl, ctx) l

let find_duplicates (lambdas: LambdaCTagged.t list) (prev_states: prev_state list):
  (LambdaTagged.t list * (Lambda.t list * print_ctx)) list =
  prev_states
  |> List.filter_map (
    fun (((ps, ps_ctx) as prev_state): prev_state): 'a option ->
      try 
        let remaining = prev_state_contained_in_state prev_state lambdas in
        Some( (remaining, ( ps, ps_ctx)) )
      with
      | _ -> None
  )
  |> List.map (fun (l1, (l2, ctx)) -> (List.map LambdaCTagged.lambdaCToLambda l1, ((List.map LambdaC.lambdaCToLambda l2), ctx)))

let eval fmt (lambda: LambdaTagged.t) = 
  let rec do_eval (states: (state * prev_state list) list) (deadlocks: state list) =
    match states with
    | [] -> List.rev deadlocks
    | (((lambdas, print_ctx) as state), prev_states)::tl -> 
      print_state fmt state;
      (* Strip LNil processes *)
      let lambdas = List.map (LambdaTagged.remLNils) lambdas in
      if (List.for_all is_LNil_or_LRepl lambdas) then
        do_eval tl deadlocks
      else (
        let reductions = eval_sync state in
        if reductions = [] then
          do_eval tl (state::deadlocks)
        else (
          let lambdasC =
            lambdas
            |> List.map LambdaTagged.lparToList
            |> List.flatten
            |> List.map LambdaTagged.remLNils 
            |> List.map lambdaTaggedToLambda
            |> List.map LambdaC.lambdaToLambdaC
          in
          let reductions = reductions
          |> List.map (fun r -> (r, (lambdasC, print_ctx)::prev_states))
          |> List.map (
            fun (((lambdas, ctx) as state, prev_states): (state * prev_state list) ): (state * prev_state list) list -> 
              let lambdasC =
                lambdas
                |> List.map LambdaTagged.lparToList
                |> List.flatten
                |> List.map LambdaTagged.remLNils 
                |> List.map LambdaCTagged.lambdaToLambdaC
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
                    LambdaTagged.printMode_no_nl fmt  (LambdaTagged.assocLeftList remaining) true;
                    Format.fprintf fmt " ; ";
                    Lambda.printMode_no_nl fmt (Lambda.assocLeftList common) true;
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



let main fmt (exp: LambdaTagged.t): bool * Lambda.t list * Lambda.t list (*passed act_ver * deadlocked processes * resolved process*) =
  try
    Printexc.record_backtrace true;
    (* Process Completeness Verification *)
    let act_ver = main_act_verifier (lambdaTaggedToLambda exp) in
    if false then (
      LambdaTagged.printMode fmt exp true;
      fprintf fmt "\n";
      print_act_ver fmt act_ver;
      (false, [], [])
    ) else (
      (* Ideally, we would just loop until no dealdock is found and discard the intermediary results.
         But the original implementation returns the first set of deadlocks and the fully deadlock
         resolved expression, so here we do the same. *)
      let (passed_act_ver, deadlocks, resolved) = detect_and_resolve fmt eval exp in

      if deadlocks = [] then (
        fprintf fmt "\nNo deadlocks!\n";
      ) else (
        fprintf fmt "\nDeadlocks:\n";
        List.iter (print_state fmt) deadlocks;
      );

      let (_, _, resolved) = detect_and_resolve_loop eval (passed_act_ver, deadlocks, resolved) None in

      if deadlocks <> [] then (
        fprintf fmt "Resolved: %b \n" (has_miss_acts act_ver);
        LambdaTagged.printMode fmt (List.hd resolved) true
      );
      (passed_act_ver, List.map stateToLambda deadlocks, List.map lambdaTaggedToLambda resolved)
    )
  with
  | _ -> Printexc.print_backtrace stdout; exit 1