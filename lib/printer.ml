(* Definition of printing functions for Action, Lambda and Proc types*)

open Format
open Types
open Types.Eta

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

let print_eta fmt e = 
    match e with
    | EEta(a) -> fprintf fmt "EEta(%a)" print_action a

let rec print_etalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_eta hd
    | hd::tl -> fprintf fmt "%a | " print_eta hd; print_etalist fmt tl

let rec print_lambdas fmt l =
    match l with
    | LNil -> fprintf fmt "LNil"
    | LOr(l1, l2) -> fprintf fmt "LOr(%a, %a)" print_lambdas l1 print_lambdas l2
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta e1 print_lambdas l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambdas l1 print_lambdas l2
    | LChi(el, ll) -> fprintf fmt "LChi(%a; %a)" print_etalist el print_lambdalist ll
and print_lambdalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_lambdas hd
    | hd::tl -> fprintf fmt "%a | " print_lambdas hd; print_lambdalist fmt tl

let rec print_etalist_alt fmt lst =
  match lst with
  | [] -> ()
  | hd::[] -> print_eta fmt hd
  | hd::tl -> print_eta fmt hd; printf ", "; print_etalist_alt fmt tl

let rec print_etalist_alt_simple fmt lst =
  match lst with
  | [] -> ()
  | hd::[] ->
    (match hd with
    | EEta(k) -> print_action_simple fmt k)
  | hd::tl -> 
    (match hd with
    | EEta(k) -> print_action_simple fmt k; printf ", "; print_etalist_alt_simple fmt tl)


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
