open Format

(* ------------------- TYPES -------------------- *)

type chan = char

type action =
    | AIn of chan
    | AOut of chan

(*  
type proc = 
    | PNil
    | PPref of action * proc
    | PPar of proc * proc
*)

type eta = 
    | EEta of action

type lambda = 
    | LNil
    | LList of eta * lambda
    | LChi of eta list * lambda list
    | LPar of lambda * lambda


(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Finds the index of elem in list *)
let rec find list elem i =
    match list with
    | [] -> -1 (* Not found *)
    | hd::tl -> if hd = elem then i else find list elem (i+1)

(* Removes the first occurrence of elem in list *)
let rec filter_first list elem = 
    match list with
    | [] -> []
    | hd::tl -> if hd != elem then hd::(filter_first tl elem) else tl

(* Substitutes the first element in a list equal to replaced with replacer *)
let rec subst_first list replaced replacer =
    match list with
    | [] -> []
    | hd::tl -> if hd = replaced then replacer::tl else hd::(subst_first tl replaced replacer)

(* Removes the EEta from a LChi's LList at index i_at *)
let rec correct_chi_lambda ll curr_i i_at =
    match ll with
    | hd::tl -> if curr_i = i_at then 
        (match hd with
        | LList(EEta(_), LList(EEta(a), l)) -> LList(EEta(a), l)::tl
        | LList(EEta(_), LNil) -> LNil::tl)
        else hd::(correct_chi_lambda tl (curr_i+1) i_at)

(* Similar to correct_chi_lambda, but returns EEta at index i_at instead of removing it *)
let rec find_chi_lambda chi curr_i i_at =
    match chi with
    | LChi(et, hd::tl) -> if curr_i = i_at then
        (match hd with
        | LList(a, LList(b, l)) -> a
        | LList(a, LNil) -> a)
        else find_chi_lambda (LChi(et, tl)) (curr_i+1) i_at

let build_chi elem chi =
    match chi with
    | LChi(el, ll) -> 
        let at_index = find el elem 0 in
            let nth_elem = List.nth el at_index in
                LChi(subst_first el nth_elem (find_chi_lambda chi 0 at_index), correct_chi_lambda ll 0 at_index)

(* Finds the indexes of the first pair of corresponding actions *)
let rec find_corres list dlist i j = 
    match list, dlist with
    | [], [] -> (-1, -1) (* Not found *)
    | [], dhd::dtl -> find_corres dtl dtl (j+1) (j+1)
    | hd::tl, dhd::dtl ->
        (match hd, dhd with
        | EEta(AIn(a)), EEta(AOut(b)) when a = b -> if i < j then (i,j) else (j,i)
        | EEta(AOut(a)), EEta(AIn(b)) when a = b -> if i < j then (i,j) else (j,i)
        | _, _ -> find_corres tl dlist (i+1) j)

let next_etas_chi chi =
    match chi with
    | LChi(el, ll) ->
        let at_indexes = find_corres el el 0 0 in
        match at_indexes with
        | (a, b) -> 
            let nth_elemA = List.nth el a in 
                let nth_elemB = List.nth el b in
                    let chi_lambdaA = find_chi_lambda chi 0 a in
                        let chi_lambdaB = find_chi_lambda chi 0 b in
                            LChi(subst_first (subst_first el nth_elemA chi_lambdaA) nth_elemB chi_lambdaB , correct_chi_lambda (correct_chi_lambda ll 0 a) 0 b)
    
(* Falta fazer todos os casos com lhs e rhs trocados *)
let rec eval_par lhs rhs =
    match lhs, rhs with
    | LList(EEta(a),l1), LList(EEta(b),l2) ->
        begin
            match a, b with
            | AIn(k), AOut(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* LChi([EEta(a);EEta(b)], [l1::[]; l2::[]]) *)
            | AOut(k), AIn(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* LChi([EEta(a);EEta(b)], [l1::[]; l2::[]]) *)
            | _, _ -> LChi([EEta(a);EEta(b)], l1::l2::[])
        end
    | LList(EEta(a), l1), LChi(EEta(b)::EEta(c)::l2, l3) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))
            | AOut(k), AIn(j), _, _ when k = j -> LPar(l1, build_chi (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3)))
            | AIn(k), AOut(j), _, _ when k = j -> LPar(l1, build_chi (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3)))
        end
    | _, _ -> LPar(lhs, rhs)
(*
let rec eval exp = match exp with
    | LNil -> LNil
    | LList(e, l) -> LList(eval e, eval l)
    | LPar(l1, l2) -> let e1 = eval l1 in let e2 = eval l2 in eval_par e1 e2
    | LError -> LError
;;
*)

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
(* let a = eval_par lhs rhs (* Tira os a's porque correspodem *) *)
let b = LChi([EEta(AIn('a')); EEta(AOut('a'))], [LNil; LNil])

(* eval_par 2nd match *)
let lhs1 = LList(EEta(AIn('b')), LNil) (* b?.0 *)
let rhs1 = LChi([EEta(AIn('a'));EEta(AOut('a'))] , [LChi([EEta(AIn('b')); EEta(AOut('b'))] , [LNil; LNil]) ; LNil]) (* (a? | a!; (b? | b!; 0; 0); 0) *)
(* let c = eval_par lhs1 rhs1;; *)
(* print_lambdas Format.std_formatter c;; *)

(* findInChi for 3rd match *)
(* chi = (a? | b! ; c!d?; b!b!) *)
let lhs2 = LList(EEta(AOut('a')), LList(EEta(AIn('b')), LNil))
let rhs2 = LChi([EEta(AIn('a')); EEta(AOut('b'))], 
                [LList(EEta(AOut('c')), LList(EEta(AIn('d')), LNil)); LList(EEta(AOut('b')), LList(EEta(AOut('b')), LNil))])
let elem = EEta(AIn('a'))                
(*let d = build_chi elem rhs2;; *)
let d = eval_par lhs2 rhs2;; 
print_lambdas Format.std_formatter d;;  

(*
let chi_ex = LChi(
    [EEta(AIn('a')); EEta(AIn('b')); EEta(AOut('c')); EEta(AOut('b'))] , 
    [LList(EEta(AOut('a')), LNil) ;  LList(EEta(AIn('d')), LNil); LList(EEta(AIn('z')), LNil) ;  LList(EEta(AOut('d')), LNil)])
let chi_ex_test = next_etas_chi chi_ex;;
print_lambdas Format.std_formatter chi_ex_test;;
*)


