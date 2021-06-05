open Format

type chan = char

type action =
    | AIn of chan
    | AOut of chan
  
type proc = 
    | PNil
    | PPref of action * proc
    | PPar of proc * proc

type eta = 
    | EEta of action

type lambda = 
    | LNil
    | LList of eta * lambda
    | LChi of eta list * lambda list
    | LPar of lambda * lambda
    | LError


let rec eval_par lhs rhs =
    match lhs, rhs with
    | LList(EEta(a),l1), LList(EEta(b),l2) -> 
        match a, b with
        | AIn(k), AOut(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* LChi([EEta(a);EEta(b)], [l1::[]; l2::[]]) *)
        | AOut(k), AIn(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* LChi([EEta(a);EEta(b)], [l1::[]; l2::[]]) *)
        | _, _ -> LChi([EEta(a);EEta(b)], l1::l2::[])
        (*if e1 = e2 then eval_par l1 l2 else
            if e1 != e2 then LChi([e1,e2],[l1,l2]) *)
;;

(*
let rec eval exp = match exp with
    | LNil -> LNil
    | LList(e, l) -> LList(eval e, eval l)
    | LPar(l1, l2) -> let e1 = eval l1 in let e2 = eval l2 in eval_par e1 e2
    | LError -> LError
;;
*)
(* ------------------- AUXILIARY FUNCTIONS -------------------- *)
let isEta e =
    match e with
    | EEta(_) -> true
    | _ -> false

let isLambda l =
    match l with
    | LNil | LList(_,_) | LChi(_,_) | LPar(_,_) -> true
    | _ -> false

let rec isLambdaList ll = 
    match ll with
    | [] -> true
    | hd::tl -> if isLambda hd then isLambdaList tl else false


(* ------------------- PRINTER -------------------- *)
let print_action fmt a =
    match a with
    | AIn(a) -> fprintf fmt "AIn(%c)" a 
    | AOut(a) -> fprintf fmt "AOut(%c)" a 

let print_eta fmt e = 
    match e with
    | EEta(a) -> fprintf fmt "EEta(%a)" print_action a

let rec print_etalist fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> fprintf fmt " %a" print_eta hd
    | hd::tl -> fprintf fmt "%a |" print_eta hd; print_etalist fmt tl


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


(* ------------------- TESTING -------------------- *)

(*let test_proc = PPar(PPref(AIn('a'), PNil), PPref(AOut('a'), PNil)) *)
let test_proc = LPar(LNil, LNil)
let lhs = LList(EEta(AIn('a')), LList(EEta(AIn('b')), LNil))
let rhs = LList(EEta(AOut('a')), LList(EEta(AOut('b')), LNil))
let a = eval_par lhs rhs (* Tira os a's porque correspodem *)
let b = LChi([EEta(AIn('a')); EEta(AOut('a'))], [LNil; LNil])
;;

print_lambdas Format.std_formatter b
(*eval test_proc;;*)

