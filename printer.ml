(* Definition of printing functions for Action, Lambda and Proc types*)

open Format
open Types
open Auxfunctions

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
    | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" print_eta e1 print_lambdas l1
    | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambdas l1 print_lambdas l2
    | LChi(el, ll) -> fprintf fmt "LChi(%a; %a)" print_etalist el print_lambdalist ll
and print_lambdalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt "%a" print_lambdas hd
    | hd::tl -> fprintf fmt "%a, " print_lambdas hd; print_lambdalist fmt tl

(* ----------- Proc ----------- *)

let rec print_proc fmt p =
    match p with
    | PNil -> fprintf fmt "PNil"
    | PPref(a, pp) -> fprintf fmt "PPref(%a, %a)" print_action a print_proc pp
    | PPar(p1, p2) -> fprintf fmt "PPar(%a, %a)" print_proc p1 print_proc p2

let rec print_proc_simple fmt p =
    match p with
    | PNil -> fprintf fmt "0"
    | PPref(a, pp) -> fprintf fmt "%a.%a" print_action_simple a print_proc_simple pp
    | PPar(p1, p2) -> fprintf fmt "(%a || %a)" print_proc_simple p1 print_proc_simple p2

(* ----------- Misc ----------- *)

let printFinalArr fmt lst =
    if !verbose then
        let i = ref 0 in List.map (fun x -> i := !i+1; printf "---- %d ----\n" !i;List.map (fun y -> print_lambdas fmt y; printf "\n") x; printf "\n\n") lst
    else ()::[]

let printMode fmt exp =
    if !verbose then
        let _ = print_lambdas fmt exp in let _ = printf " ---> " in let _ = print_proc_simple fmt (toProc exp) in printf "\n"
    else if !simplified then
        let _ = print_proc_simple fmt (toProc exp) in printf "\n"
    else ()

let rec print_list fmt lst =
    let rec lambda_list lst =
        match lst with
        | [] -> ()
        | hd::[] -> print_lambdas fmt hd
        | hd::tl -> print_lambdas fmt hd; printf "; "; lambda_list tl
    in
    printf "[";
    lambda_list lst;
    printf "]\n"

let rec print_findings lst =
    let rec find_list lst =
        match lst with
        | [] -> ()
        | hd::[] -> printf "%d" hd
        | hd::tl -> printf "%d" hd; printf ", "; find_list tl
    in 
    printf "\nThe process has a deadlock: Permutation(s) ";
    find_list lst;
    printf " lead to a deadlock.\n";

