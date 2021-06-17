open Format
open Types
open Printer
open Auxfunctions


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse

(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Defines the joining of two LChi or between a LList and a LChi *)
let join_chis lhs rhs = 
    match lhs, rhs with
    | LChi(le, ll), LChi(re, rl) -> LChi(le@re, ll@rl)
    | LChi(le, ll), LList(e, lst) -> LChi(le@[e], ll@[lst])
    | LList(e, lst), LChi(le, ll) -> LChi(le@[e], ll@[lst])
    | _, _ -> raise(RuntimeException "join_chi failed: Not a chi or list.")

(* Eliminates an EEta and its sufix in hd::tl and hd1::tl1, respectively, and returns the reduced LChi *)
let rec reduceChi chi i_at i =
    match chi with
    | LChi(hd::tl, hd1::tl1) ->
        if i = i_at then (LChi(tl, tl1)) else join_chis (LChi([hd],[hd1])) (reduceChi (LChi(tl, tl1)) i_at (i+1))

(* Removes the EEta from a LChi's LList at index i_at *)
(* -> Pode ser condensado <-*)
let rec correct_chi_lambda ll curr_i i_at =
    match ll with
    | [] -> raise (RuntimeException "correct_chi_lambda failed: Empty lambda list")
    | hd::tl -> if curr_i = i_at then 
        (match hd with
        | LList(EEta(_), LList(EEta(a), l)) -> LList(EEta(a), l)::tl
        | LList(EEta(_), LNil) -> LNil::tl
        | LList(EEta(_), LPar(m1, m2)) -> LPar(m1,m2)::tl
        | LNil -> tl
        | _ -> print_lambdas fmt hd ;raise (RuntimeException "correct_chi_lambda failed: No match "))
        else hd::(correct_chi_lambda tl (curr_i+1) i_at)

(* Similar to correct_chi_lambda, but returns EEta at index i_at instead of removing it *)
let rec find_chi_lambda chi curr_i i_at =
    match chi with
    | LChi(et, hd::tl) -> if curr_i = i_at then
        (match hd with
        | LList(a, LList(b, l)) -> a
        | LList(a, LNil) -> a
        | LList(a, LPar(_, _)) -> a
        | _ -> print_lambdas fmt hd ;raise (RuntimeException "find_chi_lambda failed: Unexpected match")) (* Pode ser preciso para o 3º caso*)
        else find_chi_lambda (LChi(et, tl)) (curr_i+1) i_at

(* Pulls the next Eta and arranges Chi's ll*)
let case_e elem chi =
    match chi with
    | LChi(el, ll) -> 
        let at_index = find el elem in
            let nth_elem = List.nth el at_index in
                let nth_ll = List.nth ll at_index in
                    match nth_ll with
                    | LNil -> LChi(List.filteri (fun i _ -> if i!=at_index then true else false) el, List.filteri (fun i _ -> if i!=at_index then true else false) ll) 
                    | _ -> LChi(subst_first el nth_elem (find_chi_lambda chi 0 at_index), correct_chi_lambda ll 0 at_index)

(* Finds and returns the indexes of the first pair of corresponding actions *)
(* The smallest index will always be on the left side of the pair *)
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
            let elemA_isNil = List.nth ll a in
            let elemB_isNil = List.nth ll b in
            if elemA_isNil = LNil || elemB_isNil = LNil then
                match elemA_isNil, elemB_isNil with
                | LNil, LNil -> reduceChi (reduceChi chi a 0) (b-1) 0
                | LNil, _ -> let r_chi = reduceChi chi a 0 in
                             (match r_chi with
                             | LChi(el1, ll1) -> let chi_lambdaB = find_chi_lambda r_chi 0 (b-1) in (LChi(subst_first el1 nth_elemB chi_lambdaB, correct_chi_lambda ll1 0 (b-1))))
                | _, LNil -> let r_chi = reduceChi chi b 0 in
                             (match r_chi with
                             | LChi (el1, ll1) -> let chi_lambdaA = find_chi_lambda r_chi 0 a in (LChi(subst_first el1 nth_elemA chi_lambdaA, correct_chi_lambda ll1 0 a)))
            else
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

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LChi at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_chi elem chi =
    match chi with
    | LChi(el, ll) -> 
        let i_at = find el elem in
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
        let i_at = find el elem in
            let f_el = List.filteri ( fun i _ -> if i!=i_at then true else false) el in
                let f_ll = List.filteri ( fun i _ -> if i!=i_at then true else false) ll in
                    join_chis (LChi(f_el, f_ll)) (get_chi_at ll i_at)

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LPar at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_par elem chi =
    match chi with
    | LChi(el, ll) -> 
        let i_at = find el elem in
            if i_at = -1 then false else
                let res = List.filteri(
                    fun i a -> 
                        if i = i_at then 
                            match a with
                            | LPar(_, _) -> true
                            | _ -> false
                        else false) ll in 
                            if List.length res = 1 then true else false

(* Used for cases where a LPar is nested inside a LChi *)
let rec lparToChi lpar =
    match lpar with
    | LPar(m1, m2) ->
            (match m1, m2 with
            | LNil, LNil -> LNil
            | LList(e1, l1), LList(e2, l2) -> LChi([e1;e2], [l1; l2])
            | LChi(_, _), LList(_, _) | LList(_, _), LChi(_, _) -> join_chis m1 m2
            | LPar(m11, m22), LList(e2, l2) -> 
                let currExp exp =
                        lparToChi m1
                in if (currExp m1) = LNil then m2 else join_chis (currExp m1) m2
            | LList(e2, l2), LPar(m11, m22) ->
                let currExp exp =
                        lparToChi m2
                in if (currExp m2) = LNil then m1 else join_chis m1 (currExp m2)
            | LNil, LPar(_, _) -> lparToChi m2
            | LPar(_,_), LNil -> lparToChi m1
            | LNil, _ -> m2
            | _, LNil -> m1)
    | _ -> raise (RuntimeException "lparToChi failed: Not a LPar.")

(* For the case where a parallel composition is prefixed by some other action *)
(* Example case: a?.0 x (a! | b?; (b!.0 || c?.0); 0) -> (b! || c? || b?; 0; 0; 0) *)
let case_h elem chi =
    match chi with
    | LChi(el, ll) ->
        let i_at = find el elem in
            let f_el = List.filteri ( fun i _ -> if i!=i_at then true else false) el in
                let f_ll = List.filteri ( fun i _ -> if i!=i_at then true else false) ll in
                    join_chis (LChi(f_el, f_ll)) (lparToChi (get_chi_at ll i_at))


(* Retrieves the lambdas from a LPar type and adds them to a list *)
let rec lparToList exp = 
    match exp with
    | LPar(l, r) -> lparToList l @ lparToList r
    | _ -> [exp]

(* Source: https://stackoverflow.com/questions/46121765/generating-all-permutations-in-a-functional-language *)
let ( ^^ ) e ll = List.map (fun x -> e::x) ll
let rec permut l r = 
    match r with 
    | [] -> [[]]
    | [x] -> x ^^ (permut [] l)
    | x::t -> let s = permut (x::l) t in 
              (x ^^ (permut [] (l@t))) @ s;;

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
                | LNil, _ -> LPar(lhs, lc)                                      (* Case C *)
                | _, LNil -> LPar(lhs, lb))                                     (* Case D *)
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lhs, lc)                                      (* Case C *)
                | _, LNil -> LPar(lhs, lb))                                     (* Case D *)
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) rhs -> LPar(l1, case_g (EEta(AIn(k))) rhs)    (* Case G *)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) rhs -> LPar(l1, case_g (EEta(AOut(k))) rhs)   (* Case G *)
            | AOut(k), _, _, _ when exists_and_par (EEta(AIn(k))) rhs -> LPar(l1, case_h (EEta(AIn(k))) rhs)
            | AIn(k), _, _, _ when exists_and_par (EEta(AOut(k))) rhs -> LPar(l1, case_h (EEta(AOut(k))) rhs)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(LList(EEta(a), l1), case_f (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case F *)
            | AOut(k), _ , _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                   then LPar(l1, case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
                                   else join_chis lhs rhs
            | AIn(k), _ , _, _  -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                   then LPar(l1, case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
                                   else join_chis lhs rhs
        end
    | LChi(EEta(b)::EEta(c)::l2, l3), LList(EEta(a), l1) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lc, rhs)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lb, rhs)
                | LList(d,e), LNil -> LPar(lb, rhs)
                | LNil, LList(d,e) -> LPar(lc, rhs))
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lc, rhs)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lb, rhs)
                | LList(d,e), LNil -> LPar(lb, rhs)
                | LNil, LList(d,e) -> LPar(lc, rhs))
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) lhs -> LPar(case_g (EEta(AIn(k))) lhs, l1)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) lhs -> LPar(case_g (EEta(AOut(k))) lhs, l1)
            | AOut(k), _, _ ,_ when exists_and_par (EEta(AIn(k))) lhs -> LPar(case_h (EEta(AIn(k))) lhs, l1)
            | AIn(k), _, _, _ when exists_and_par (EEta(AOut(k))) lhs -> LPar(case_h (EEta(AOut(k))) lhs, l1)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(case_f (LChi(EEta(b)::EEta(c)::l2, l3)), LList(EEta(a), l1))
            | AOut(k), _, _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                  then LPar(case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3)), l1)
                                  else join_chis lhs rhs
            | AIn(k), _, _, _ -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                 then LPar(case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3)), l1)
                                 else join_chis lhs rhs
        end
    (* Cases where one side is a Chi with two top levels and the other is LNil *)
    (* May need to add cases where they dont match *)
    | LChi(EEta(b)::EEta(c)::l2, l3), LNil ->
        begin
            match b, c, l3 with
            | AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lc, rhs)
                | _ , LNil -> LPar(lb, rhs)
                | LNil, LNil -> LPar(LNil, LNil))
            | AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lc, rhs)
                | _, LNil -> LPar(lb, rhs)
                | LNil, LNil -> LPar(LNil, LNil))
            | AIn(k), AOut(j), _ | AOut(k), AIn(j), _ when k!=j && List.length l2 = 0 -> lhs
        end
    | LNil, LChi(EEta(b)::EEta(c)::l2, l3) ->
        begin
            match b, c, l3 with
            | AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lhs, lc)                                      
                | _ , LNil -> LPar(lhs, lb)
                | LNil, LNil -> LPar(LNil, LNil))                                     
            | AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lhs, lc)                                      
                | _ , LNil -> LPar(lhs, lb)
                | LNil, LNil -> LPar(LNil, LNil))                                     
        end
    (* Cases where lhs is a LChi with only one Eta and rhs is a LList *)
    | LChi([a], ll), LList(e, l) ->
        begin
            match a, ll, e, l with
            | _, [LNil], _, _ -> LPar(LList(a, LNil), rhs)
            | EEta(AOut(j)), _, EEta(AIn(k)), _ ->  if k = j then LPar(case_e (EEta(AOut(k))) lhs, l) else join_chis lhs rhs
            | EEta(AIn(j)), _, EEta(AOut(k)), _ -> if k = j then LPar(case_e (EEta(AIn(k))) lhs, l) else join_chis lhs rhs
            | _, _, _, _ -> join_chis lhs rhs
        end
    | LList(e, l), LChi([a], ll) ->
        begin
            match a, ll, e, l with
            | _, [LNil], _, _ -> LPar(lhs, LList(a, LNil))
            | EEta(AOut(j)), _, EEta(AIn(k)), _ -> if k = j then LPar(l, case_e (EEta(AOut(k))) rhs) else join_chis lhs rhs
            | EEta(AIn(j)), _, EEta(AOut(k)), _ -> if k = j then LPar(l, case_e (EEta(AIn(k))) rhs) else join_chis lhs rhs
            | _, _, _, _ -> join_chis lhs rhs
        end
    (* Probably only works for LLists now *)
    | LChi(el1, ll1), LChi(el2, ll2) -> 
        let rec calcChi chi =
            match chi with
            | LChi(el3, ll3) -> 
            if exist_corres el3 then (printMode fmt chi; calcChi (case_f chi)) else 
                match chi with
                | LChi([], []) -> LNil
                | LChi(_, _) -> chi
        in calcChi (join_chis lhs rhs)
    | LNil, _ -> rhs
    | _, LNil -> lhs
    | _, _ -> print_lambdas fmt (LPar(lhs,rhs));printf "\n";raise (RuntimeException "No match in eval_par\n")

let rec eval exp =
    printMode fmt exp;
    match exp with
    | LNil -> LNil
    | LList(e, l) -> exp
    | LPar(LNil, LPar(l3, l4)) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4) (* These cases exist to remove the prints *)
    | LPar(LPar(l3,l4), LNil) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4)  (* of redudant LNil evaluations *)
    | LPar(l1, l2) -> eval (let e1 = eval l1 in let e2 = eval l2 in eval_par e1 e2)
    | LChi(el, ll) -> exp

(* Receives a list of process permutations and calls eval on each one *)
(* Note: It assumes LPar is left associative. Any previous associations are discarded in permut.*)
let rec assign_eval expLst =
    match expLst with
    | [] -> []
    | hd::tl -> let r_hd = List.rev hd in (eval (arrange_perms r_hd))::(assign_eval tl)
and arrange_perms exp =
    match exp with
    | [h; t] -> printf "\n\n";LPar(t, h)
    | h::t -> LPar(arrange_perms t, h)

let main exp =
    let toList = lparToList exp in
    let res = if List.length toList <= 2 
              then (eval exp)::[] 
              else let perm_lst = permut [] toList in 
              printFinalArr fmt perm_lst; assign_eval (List.rev perm_lst)
    in
    let findings = proc_findings (List.rev res) in
    if List.length findings = List.length res
    then printf "\nThe process has a deadlock: every process permutation is blocked.\n"
    else if List.length findings = 0 
        then printf "\nThe process is deadlock-free.\n"
        else print_findings findings;
    if !verbose then let _ = printf "\n" in print_list fmt (List.rev res) else ()
;;

(* ------------------- TESTING -------------------- *)
(* main ( LPar(LPar(LList(EEta(AOut('b')), LPar(LNil, LNil)), LList(EEta(AIn('b')), LNil) ), LList(EEta(AIn('b')), LList(EEta(AOut('b')), LPar(LList(EEta(AOut('a')) , LNil) , LList(EEta(AIn('a')) , LNil))))) ) *)

(* assign_eval (List.rev (permut [] (lparToList  ( LPar( LPar( LList(EEta(AIn('a')) , LList(EEta(AIn('a')), LNil)) , LNil) , LList(EEta(AIn('a')) , LList(EEta(AOut('b')), LNil)))))))*)

main (LPar( LPar( LList(EEta(AIn('a')), LPar(LList(EEta(AOut('b')), LNil) , LList( EEta(AIn('c')), LList(EEta(AIn('d')), LNil)))) , LList(EEta(AIn('b')), LNil)) ,
LList(EEta(AOut('a')), LPar(LList(EEta(AOut('c')), LNil) , LList(EEta(AOut('d')), LNil))) ));


