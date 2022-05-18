(* Definition of printing functions for Action, Lambda and Proc types*)

open Format
open Types
open Cmd

let fmt = Format.std_formatter

(* ----------- Action ----------- *)

let print_action fmt a =
    match a with
    | AIn(a) -> fprintf fmt "AIn(%c)" a 
    | AOut(a) -> fprintf fmt "AOut(%c)" a 

let print_action_simple fmt a =
    match a with
    | AIn(a) -> fprintf fmt "%c?" a 
    | AOut(a) -> fprintf fmt "%c!" a

(* ----------- Lambda ----------- *)
let print_eta_tagged fmt (e: eta_tagged) = 
    match e with
    | EEtaTagged(a, i) -> fprintf fmt "EEta(%a, %i)" print_action a i

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
    | LOr(l1, l2) -> fprintf fmt "LOr(%a, %a)" print_lambda_tagged l1 print_lambda_tagged l2
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta_tagged e1 print_lambda_tagged l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambda_tagged l1 print_lambda_tagged l2
    | LChi(el, ll) -> fprintf fmt "LChi(%a; %a)" print_eta_tagged_list el print_lambda_tagged_list ll
    | LSubst -> fprintf fmt "LSubst"
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
    | LOr(l1, l2) -> fprintf fmt "LOr(%a, %a)" print_lambdas l1 print_lambdas l2
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta e1 print_lambdas l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambdas l1 print_lambdas l2
    | LChi(el, ll) -> fprintf fmt "LChi(%a; %a)" print_etalist el print_lambdalist ll
    | LSubst -> fprintf fmt "LSubst"
and print_lambdalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_lambdas hd
    | hd::tl -> fprintf fmt "%a, " print_lambdas hd; print_lambdalist fmt tl

let rec print_etalist_alt fmt lst =
  match lst with
  | [] -> ()
  | hd::[] -> print_eta fmt hd
  | hd::tl -> print_eta fmt hd; printf ", "; print_etalist_alt fmt tl

let rec print_etalist_alt_simple fmt lst =
  match lst with
  | [] -> ()
  | EEta(k)::[] ->
    print_action_simple fmt k
  | EEta(k)::tl ->
    print_action_simple fmt k; printf ", "; print_etalist_alt_simple fmt tl


(* ----------- Proc ----------- *)

let rec print_proc fmt p =
    match p with
    | PNil -> fprintf fmt "PNil"
    | POr(p1, p2) -> fprintf fmt "POr(%a, %a)" print_proc p1 print_proc p2
    | PPref(a, pp) -> fprintf fmt "PPref(%a, %a)" print_action a print_proc pp
    | PPar(p1, p2) -> fprintf fmt "PPar(%a, %a)" print_proc p1 print_proc p2

let rec print_proc_simple fmt p =
    match p with
    | PNil -> fprintf fmt "0"
    | POr(p1, p2) -> fprintf fmt "(%a + %a)" print_proc_simple p1 print_proc_simple p2
    | PPref(a, pp) -> fprintf fmt "%a.%a" print_action_simple a print_proc_simple pp
    | PPar(p1, p2) -> fprintf fmt "(%a || %a)" print_proc_simple p1 print_proc_simple p2

(* ----------- Misc ----------- *)

let printMode fmt exp p =
  if p then
    if !verbose then (
      print_lambdas fmt exp;
      printf " ---> ";
      print_proc_simple fmt (toProc exp);
      printf "\n"
    )else if !simplified then(
      print_proc_simple fmt (toProc exp);
      printf "\n"
    )

let rec print_list_comb fmt lst =
    let rec find_list lst =
        match lst with
        | [] -> ()
        | hd::[] -> 
          (match hd with 
          | (l, c) -> 
            if !verbose 
            then (printf "("; print_lambdas fmt l; printf ", %s)" c.level; printf "]";printf " ---> ";print_proc_simple fmt (toProc l))
            else (printf "("; print_proc_simple fmt (toProc l); printf ", %s)" c.level; printf "]"))
        | hd::tl -> 
          (match hd with 
          | (l, c) -> 
            if !verbose
            then (printf "("; print_lambdas fmt l; printf ", %s)" c.level; printf "; ---> ";print_proc_simple fmt (toProc l);printf "\n"; find_list tl)
            else (printf "("; print_proc_simple fmt (toProc l); printf ", %s)" c.level; printf "\n"; find_list tl))
    in
    printf "\n[";
    find_list lst;
    printf "\n"

let printFinalArrComb fmt lst =
    if !verbose then
        (printf "Initial Combinations:\n";
        let i = ref 0 in List.iter (fun x -> i:= !i+1; printf "---- %d ----\n" !i; printMode fmt x true; printf "\n") lst)
    else ()

let count_level print_ctx =
  let count = ref 0 in
    String.iter (fun c -> if c = '.' then count := !count + 1) print_ctx.level;
    !count

let printCtxLevel_noln p_ctx =
  if p_ctx.print then
    let level = count_level p_ctx in
    let level_indent = String.make (level*2) ' ' in
    if !verbose || !simplified then printf "---- %s ----\n" p_ctx.level else ()
  else ()

let printCtxLevel p_ctx =
  if p_ctx.print then
    if !verbose || !simplified then printf "\n---- %s ----\n" p_ctx.level else ()
  else ()

let printCtxLevel1 p_ctx eta et i =
  if p_ctx.print then
    if !verbose || !simplified then (printf "\n---- %s ---- -> between " p_ctx.level; print_eta fmt eta; printf " at i = %d and " i; print_eta fmt et; printf "\n") else ()
  else ()

let printCtxLevel2 p_ctx el p =
  if p_ctx.print then
    match p with
    | (a,b) -> if !verbose || !simplified then (printf "\n---- %s ---- -> between " p_ctx.level; print_eta fmt (List.nth el a); printf " at i = %d and " a; print_eta fmt (List.nth el b);printf " at j = %d" b;printf "\n") else ()
  else ()

(* ------------------- BEGIN CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let rec print_act_ver arr =
  let rec print arr =
    match arr with
    | [] -> ()
    | hd::tl -> 
        match hd with
        | (a, []) -> print tl
        | (a, b) -> 
          if !verbose then
            (printf "- "; print_etalist_alt fmt b; printf " in "; printMode fmt a true; printf "\n"; print tl)
          else (
            if !simplified then
              (printf "- "; print_etalist_alt_simple fmt b; printf " in "; printMode fmt a true; print tl)
          )
  in
  if !verbose || !simplified then printf "Action(s) missing correspondence(s) in process(es):\n"; print arr