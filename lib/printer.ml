(* Definition of printing functions for Action, Lambda and Proc types*)

open Format
open Types
open Cmd

let null_fmt = Format.make_formatter ( fun _ _ _ -> () ) (fun _ -> ())

(* ----------- Action ----------- *)

let print_action fmt a =
    match a with
    | AIn(a) -> fprintf fmt "AIn(%s)" a
    | AOut(a) -> fprintf fmt "AOut(%s)" a

let print_action_simple fmt a =
    match a with
    | AIn(a) -> fprintf fmt "%s?" a
    | AOut(a) -> fprintf fmt "%s!" a

(* ----------- Lambda ----------- *)
let print_eta_tagged fmt (e: eta_tagged) = 
    match e with
    | EEtaTagged(a, i) -> fprintf fmt "EEtaTagged(%a, %i)" print_action a i

let print_eta fmt e = 
    match e with
    | EEta(a) -> fprintf fmt "EEta(%a)" print_action a

let rec print_etalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_eta hd
    | hd::tl -> fprintf fmt "%a | " print_eta hd; print_etalist fmt tl

let rec print_lambda_tagged fmt l =
    match l with
    | LNil -> fprintf fmt "LNil"
    | LOrI(l1, l2) -> fprintf fmt "LOrI(%a, %a)" print_lambda_tagged l1 print_lambda_tagged l2
    | LOrE(l1, l2) -> fprintf fmt "LOrE(%a, %a)" print_lambda_tagged l1 print_lambda_tagged l2
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta_tagged e1 print_lambda_tagged l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambda_tagged l1 print_lambda_tagged l2
    | LRepl(e1, l1) -> fprintf fmt "LRepl(%a, %a)" print_eta_tagged e1 print_lambda_tagged l1
and print_eta_tagged_list fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_eta_tagged hd
    | hd::tl -> fprintf fmt "%a | " print_eta_tagged hd; print_eta_tagged_list fmt tl
and print_lambda_tagged_list fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_lambda_tagged hd
    | hd::tl -> fprintf fmt "%a, " print_lambda_tagged hd; print_lambda_tagged_list fmt tl

let rec print_lambdas fmt l =
    match l with
    | LNil -> fprintf fmt "LNil"
    | LOrI(l1, l2) -> fprintf fmt "LOrI(%a, %a)" print_lambdas l1 print_lambdas l2
    | LOrE(l1, l2) -> fprintf fmt "LOrE(%a, %a)" print_lambdas l1 print_lambdas l2
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta e1 print_lambdas l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambdas l1 print_lambdas l2
    | LRepl(e1, l1) -> fprintf fmt "LRepl(%a, %a)" print_eta e1 print_lambdas l1
and print_lambdalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_lambdas hd
    | hd::tl -> fprintf fmt "%a, " print_lambdas hd; print_lambdalist fmt tl

let rec print_etalist_alt fmt lst =
  match lst with
  | [] -> ()
  | hd::[] -> print_eta fmt hd
  | hd::tl -> print_eta fmt hd; fprintf fmt ", "; print_etalist_alt fmt tl

let rec print_etalist_alt_simple fmt lst =
  match lst with
  | [] -> ()
  | EEta(k)::[] ->
    print_action_simple fmt k
  | EEta(k)::tl ->
    print_action_simple fmt k; fprintf fmt ", "; print_etalist_alt_simple fmt tl



let rec print_lambda_simple fmt l =
    match l with
    | LNil -> fprintf fmt "0"
    | LOrI(p1, p2) -> fprintf fmt "(%a + %a)" print_lambda_simple p1 print_lambda_simple p2
    | LOrE(p1, p2) -> fprintf fmt "(%a & %a)" print_lambda_simple p1 print_lambda_simple p2
    | LList(EEta(a), pp) -> fprintf fmt "%a.%a" print_action_simple a print_lambda_simple pp
    | LPar(p1, p2) -> fprintf fmt "(%a || %a)" print_lambda_simple p1 print_lambda_simple p2
    | LRepl(EEta(a), pp) -> fprintf fmt "*%a.%a" print_action_simple a print_lambda_simple pp

(* ----------- Misc ----------- *)

let printMode_base nl fmt exp p =
  if p then (
    if !verbose then (
      print_lambdas fmt exp;
      fprintf fmt " ---> ";
      print_lambda_simple fmt exp;
    ) else if !simplified then(
      print_lambda_simple fmt exp;
    );
    if nl then fprintf fmt "\n"
  )

let printMode = printMode_base true
let printMode_no_nl = printMode_base false

let count_level print_ctx =
  let count = ref 0 in
    String.iter (fun c -> if c = '.' then count := !count + 1) print_ctx.level;
    !count

let printCtxLevel_noln fmt p_ctx =
  if p_ctx.print then
    let level = count_level p_ctx in
    if !verbose || !simplified then
      fprintf fmt "---- %s ----\n" p_ctx.level
  else ()

let printCtxLevel fmt p_ctx =
  if p_ctx.print then
    if !verbose || !simplified then
      fprintf fmt "\n---- %s ----\n" p_ctx.level
  else ()

let printCtxLevel1 fmt p_ctx eta et i =
  if p_ctx.print && (!verbose || !simplified) then (
    fprintf fmt "\n---- %s ---- -> between " p_ctx.level;
    print_eta fmt eta;
    fprintf fmt " at i = %d and " i;
    print_eta fmt et;
    fprintf fmt "\n"
  )

let printCtxLevel2 fmt p_ctx el p =
  if p_ctx.print then
    match p with
    | (a,b) ->
      if !verbose || !simplified then (
        fprintf fmt "\n---- %s ---- -> between " p_ctx.level;
        print_eta fmt (List.nth el a);
        fprintf fmt " at i = %d and " a;
        print_eta fmt (List.nth el b);
        fprintf fmt " at j = %d" b;
        fprintf fmt "\n"
      )

(* ------------------- BEGIN CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let print_act_ver fmt arr =
  let rec print arr =
    match arr with
    | [] -> ()
    | hd::tl -> 
        match hd with
        | (a, []) -> print tl
        | (a, b) -> 
          if !verbose then (
            fprintf fmt "- ";
            print_etalist_alt fmt b;
            fprintf fmt " in ";
            printMode fmt a true;
            fprintf fmt "\n";
            print tl
          ) else if !simplified then (
            (fprintf fmt "- ";
            print_etalist_alt_simple fmt b;
            fprintf fmt " in ";
            printMode fmt a true;
            print tl)
          )
  in
  if !verbose || !simplified then
    fprintf fmt "Action(s) missing correspondence(s) in process(es):\n";
    print arr