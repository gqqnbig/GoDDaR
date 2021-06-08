open Format

(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

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


(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Finds the index of elem in list *)
let rec find list elem i =
    match list with
    | [] -> -1 (* Not found *)
    | hd::tl -> if hd = elem then i else find tl elem (i+1)

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
    | [] -> raise (RuntimeException "correct_chi_lambda failed: Empty lambda list")
    | hd::tl -> if curr_i = i_at then 
        (match hd with
        | LList(EEta(_), LList(EEta(a), l)) -> LList(EEta(a), l)::tl
        | LList(EEta(_), LNil) -> LNil::tl
        | _ -> raise (RuntimeException "correct_chi_lambda failed: No match"))
        else hd::(correct_chi_lambda tl (curr_i+1) i_at)

(* Similar to correct_chi_lambda, but returns EEta at index i_at instead of removing it *)
let rec find_chi_lambda chi curr_i i_at =
    match chi with
    | LChi(et, hd::tl) -> if curr_i = i_at then
        (match hd with
        | LList(a, LList(b, l)) -> a
        | LList(a, LNil) -> a
        | _ -> raise (RuntimeException "find_chi_lambda failed: Unexpected match")) (* Pode ser preciso para o 3º caso*)
        else find_chi_lambda (LChi(et, tl)) (curr_i+1) i_at

(* Pulls the next Eta and arranges Chi's ll*)
let case_e elem chi =
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

(* Assuming two Etas in a Chi's el are equal, this function pulls the two next Etas and arranges the Chi's ll *)
(* Example case: chi = (a? | b? | c! | b!; a!, d?, z?, d!) *)
let case_f chi =
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

(* Checks whether there are corresponding actions in list *)
let rec exist_corres list =
    match list with
    | [] -> false
    | hd::tl -> 
        match hd with
        | EEta(AIn(a)) -> List.exists ((=) (EEta(AOut(a)))) tl || exist_corres tl
        | EEta(AOut(a)) -> List.exists ((=) (EEta(AIn(a)))) tl || exist_corres tl

(* Defines the joining of two LChi *)
let join_chis lchi rchi = 
    match lchi, rchi with
    | LChi(le, ll), LChi(re, rl) -> LChi(le@re, ll@rl)
    | _, _ -> raise(RuntimeException "join_chi failed: Not a chi.")

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LChi at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_chi elem chi =
    match chi with
    | LChi(el, ll) -> 
        let i_at = find el elem 0 in
            if i_at = -1 then false else
                let res = List.filteri(
                    fun i a -> 
                        if i = i_at then 
                            match a with
                            | LChi(_, _) -> true
                            | _ -> false
                        else false) ll in 
                            if List.length res = 1 then true else false

(* Retrieves the LChi at position i_at in list *)
let get_chi_at list i_at =
    let f_list = List.filteri (fun i _ -> if i = i_at then true else false) list in (List.hd f_list)

(* Defines the case when there are two correspondent actions and a Chi must be pulled from the level below *)
(* Example case: a?0 x (b! | a!; 0, (c! | d?; 0, 0)) -> 0 x (b! | c! | d?; 0; 0; 0) *)
let case_g elem chi =
    match chi with
    | LChi(el, ll) ->
        let i_at = find el elem 0 in
            let f_el = List.filteri ( fun i _ -> if i!=i_at then true else false) el in
                let f_ll = List.filteri ( fun i _ -> if i!=i_at then true else false) ll in
                    join_chis (LChi(f_el, f_ll)) (get_chi_at ll i_at)

(* Function that contains all the reduction cases *)
(* Falta fazer todos os casos com lhs e rhs trocados *)
let eval_par lhs rhs =
    match lhs, rhs with
    | LList(EEta(a),l1), LList(EEta(b),l2) ->
        begin
            match a, b with
            | AIn(k), AOut(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B*)
            | AOut(k), AIn(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B *)
            | _, _ -> LChi([EEta(a);EEta(b)], l1::l2::[])                                            (* Case B *)
        end
    | LList(EEta(a), l1), LChi(EEta(b)::EEta(c)::l2, l3) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)                                      (* Case C *)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))                                     (* Case D *)
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)                                      (* Case C *)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))                                     (* Case D *)
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) rhs -> LPar(l1, case_g (EEta(AIn(k))) rhs)    (* Case G *)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) rhs -> LPar(l1, case_g (EEta(AOut(k))) rhs)   (* Case G *)
            | AOut(k), _ , _, _ when List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) -> LPar(l1, case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
            | AIn(k), _ , _, _ when List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2) -> LPar(l1, case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(LList(EEta(a), l1), case_f (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case F *)
        end
    | _, _ -> LPar(lhs, rhs) (* Results in infinite loop *)

let print_return exp = print_lambdas Format.std_formatter exp; printf " aa\n"; exp

let rec eval exp =
    print_lambdas Format.std_formatter exp; printf "\n";
    match exp with
    | LNil -> print_return (LNil)
    | LList(e, l) -> exp
    | LPar(LNil, LNil) -> LNil
    | LPar(LNil, LPar(l3, l4)) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4)
    | LPar(LNil, l2) -> print_return l2
    | LPar(LPar(l3,l4), LNil) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4)
    | LPar(l1, LNil) -> print_return l1
    | LPar(l1, l2) -> eval (let e1 = eval l1 in let e2 = eval l2 in eval_par e1 e2)
    | LChi(el, ll) -> LChi(el, ll)

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
(* let d = eval_par lhs2 rhs2;; *)
(* print_lambdas Format.std_formatter d;;  *)

let lhs_ex = LList(EEta(AIn('c')), LNil)
let chi_ex = LChi(
    [EEta(AIn('a')); EEta(AIn('b')); EEta(AOut('c')); EEta(AOut('b'))] , 
    [LList(EEta(AOut('a')), LNil) ;  LList(EEta(AIn('d')), LNil); LList(EEta(AIn('z')), LNil) ;  LList(EEta(AOut('d')), LNil)])
(* let chi_ex_test = next_etas_chi chi_ex;; *)
(* let chi_ex_test = eval_par lhs_ex chi_ex;; *)
(* let chi_ex_test = build_chi (EEta(AIn('b'))) chi_ex;; *)
(* let chi_ex_test = eval (LPar(lhs_ex, chi_ex));;
print_lambdas Format.std_formatter chi_ex_test;; *)
;;

print_lambdas Format.std_formatter (eval (LPar(LList(EEta(AOut('a')), LPar(LList(EEta(AOut('b')), LNil) , LList(EEta(AIn('b')), LNil))), LList(EEta(AIn('a')), LNil))));;


