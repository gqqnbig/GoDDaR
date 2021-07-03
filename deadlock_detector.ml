open Format
open Types
open Printer
open Auxfunctions


(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

(* ------------------- COMMAND LINE -------------------- *)

(* let () = cmdParse *)

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

(* Used for cases where a LPar is nested inside a LChi *)
let rec lparToChi lpar =
    match lpar with
    | LPar(m1, m2) ->
            (match m1, m2 with
            | LNil, LNil -> LNil
            | LList(e1, l1), LList(e2, l2) -> LChi([e1;e2], [l1; l2])
            | LChi(_, _), LList(_, _) | LList(_, _), LChi(_, _) -> join_chis m1 m2
            | LPar(m11, m22), LList(e2, l2) -> 
                let currExp exp = lparToChi m1 in 
                if (currExp m1) = LNil then m2 else join_chis (currExp m1) m2
            | LList(e2, l2), LPar(m11, m22) ->
                let currExp exp = lparToChi m2 in
                if (currExp m2) = LNil then m1 else join_chis m1 (currExp m2)
            | LNil, LPar(_, _) -> lparToChi m2
            | LPar(_,_), LNil -> lparToChi m1
            | LNil, _ -> m2
            | _, LNil -> m1)
    | _ -> print_lambdas fmt lpar ;raise (RuntimeException "lparToChi failed: Not a LPar.")

(* Retrieves the LChi at position i_at in list *)
let get_chi_at list i_at =
    let f_list = List.filteri (fun i _ -> if i = i_at then true else false) list in (List.hd f_list)

(* Pulls the next Eta and arranges Chi's ll*)
let case_e elem chi i_at =
    match chi with
    | LChi(el, ll) -> 
        let at_index = if i_at = -1 then find el elem else i_at in
            let nth_elem = List.nth el at_index in
                let nth_ll = List.nth ll at_index in
                    match nth_ll with
                    | LNil -> LChi(List.filteri (fun i _ -> if i!=at_index then true else false) el, List.filteri (fun i _ -> if i!=at_index then true else false) ll) 
                    | _ when i_at = -1-> LChi(subst_first el nth_elem (find_chi_lambda chi 0 at_index), correct_chi_lambda ll 0 at_index)
                    | _ when i_at != -1 -> LChi(subst_at el (find_chi_lambda chi 0 at_index) 0 at_index, correct_chi_lambda ll 0 at_index)

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

(* Falta fazer casos para LCHI *)
(* Assuming two Etas in a Chi's el are equal, this function pulls the two next Etas and arranges the Chi's ll *)
(* Example case: chi = (a? | b? | c! | b!; a!, d?, z?, d!) *)
let case_f chi i_pair =
    match chi with
    | LChi(el, ll) ->
        let at_indexes = if i_pair = (-1,-1) then find_corres el el 0 0 else i_pair in
        match at_indexes with
        | (a, b) -> 
            let elemA_el = List.nth el a in 
            let elemB_el = List.nth el b in
            let elemA_ll = List.nth ll a in
            let elemB_ll = List.nth ll b in
            if elemA_ll = LNil || elemB_ll = LNil then
                (match elemA_ll, elemB_ll with
                | LNil, LNil -> reduceChi (reduceChi chi a 0) (b-1) 0;
                | LNil, LPar(_,_) -> let r_chi = reduceChi chi a 0 in
                                     let lpChi = lparToChi (get_chi_at ll b) in
                                     let rr_chi = reduceChi r_chi (b-1) 0 in
                                        join_chis rr_chi lpChi
                | LNil, _ -> let r_chi = reduceChi chi a 0 in
                             (match r_chi with
                             | LChi(el1, ll1) -> let chi_lambdaB = find_chi_lambda r_chi 0 (b-1) in (LChi(subst_at el1 chi_lambdaB 0 (b-1), correct_chi_lambda ll1 0 (b-1))))
                | LPar(_,_), LNil -> let r_chi = reduceChi chi b 0 in
                                     let lpChi = lparToChi (get_chi_at ll a) in
                                     let rr_chi = reduceChi r_chi a 0 in
                                        join_chis rr_chi lpChi
                | _, LNil -> let r_chi = reduceChi chi b 0 in
                             (match r_chi with
                             | LChi (el1, ll1) -> let chi_lambdaA = find_chi_lambda r_chi 0 a in (LChi(subst_at el1 chi_lambdaA 0 a, correct_chi_lambda ll1 0 a)))
                ) else
            let chi_lambdaA = find_chi_lambda chi 0 a in
            let chi_lambdaB = find_chi_lambda chi 0 b in
            printf "ULTIMO CASO\n";LChi(subst_first (subst_first el elemA_el chi_lambdaA) elemB_el chi_lambdaB , correct_chi_lambda (correct_chi_lambda ll 0 a) 0 b)

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

(* Defines the case when there are two correspondent actions and a Chi must be pulled from the level below *)
(* Example case: a?0 x (b! | a!; 0, (c! | d?; 0, 0)) -> 0 x (b! | c! | d?; 0; 0; 0) *)
let case_g elem chi =
    (*printf "Entrei case_g\n"; *)
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

(* For the case where a parallel composition is prefixed by some other action *)
(* Example case: a?.0 x (a! | b?; (b!.0 || c?.0); 0) -> (b! || c? || b?; 0; 0; 0) *)
let case_h elem chi at_i =
    (*printf "Entrei case_h\n"; *)
    match chi with
    | LChi(el, ll) ->
        let i_at = if at_i = -1 then find el elem else at_i in
            let f_el = List.filteri ( fun i _ -> if i!=i_at then true else false) el in
                let f_ll = List.filteri ( fun i _ -> if i!=i_at then true else false) ll in
                    join_chis (LChi(f_el, f_ll)) (lparToChi (get_chi_at ll i_at))

let rec has_lpar_in_chi chi =
    match chi with
    | LChi([], []) -> false
    | LChi(ehd::etl, lhd::ltl) ->
        match lhd with
        | LPar(_, _) -> true
        | _ -> has_lpar_in_chi (LChi(etl, ltl))

(* Function that contains all the reduction cases *)
(* Falta fazer todos os casos com lhs e rhs trocados *)
let eval_par lhs rhs =
    (*printf "entrei eval_par\n"; *)
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
            | AOut(k), _, _, _ when exists_and_par (EEta(AIn(k))) rhs -> LPar(l1, case_h (EEta(AIn(k))) rhs (-1))
            | AIn(k), _, _, _ when exists_and_par (EEta(AOut(k))) rhs -> LPar(l1, case_h (EEta(AOut(k))) rhs (-1))
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(LList(EEta(a), l1), case_f (LChi(EEta(b)::EEta(c)::l2, l3)) (-1,-1)) (* Case F *)
            | AOut(k), _ , _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                   then LPar(l1, case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3)) (-1)) (* Case E *)
                                   else join_chis lhs rhs
            | AIn(k), _ , _, _  -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                   then LPar(l1, case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3)) (-1)) (* Case E *)
                                   else join_chis lhs rhs
        end
    | LChi(EEta(b)::EEta(c)::l2, l3), LList(EEta(a), l1) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lc, rhs)
                | _, LNil -> LPar(lb, rhs))
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, _ -> LPar(lc, rhs)
                | _, LNil -> LPar(lb, rhs))
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) lhs -> LPar(case_g (EEta(AIn(k))) lhs, l1)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) lhs -> LPar(case_g (EEta(AOut(k))) lhs, l1)
            | AOut(k), _, _ ,_ when exists_and_par (EEta(AIn(k))) lhs -> LPar(case_h (EEta(AIn(k))) lhs (-1), l1)
            | AIn(k), _, _, _ when exists_and_par (EEta(AOut(k))) lhs -> LPar(case_h (EEta(AOut(k))) lhs (-1), l1)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(case_f (LChi(EEta(b)::EEta(c)::l2, l3)) (-1,-1), LList(EEta(a), l1))
            | AOut(k), _, _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                  then LPar(case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3)) (-1), l1)
                                  else join_chis lhs rhs
            | AIn(k), _, _, _ -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                 then LPar(case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3)) (-1), l1)
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
            | _, _, _ -> lhs
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
            | _, _, _ -> rhs                                     
        end
    (* Cases where lhs is a LChi with only one Eta and rhs is a LList *)
    | LChi([a], ll), LList(e, l) ->
        begin
            match a, ll, e, l with
            | _, [LNil], _, _ -> LPar(LList(a, LNil), rhs)
            | EEta(AOut(j)), _, EEta(AIn(k)), _ ->  if k = j then LPar(case_e (EEta(AOut(k))) lhs (-1), l) else join_chis lhs rhs
            | EEta(AIn(j)), _, EEta(AOut(k)), _ -> if k = j then LPar(case_e (EEta(AIn(k))) lhs (-1), l) else join_chis lhs rhs
            | _, _, _, _ -> join_chis lhs rhs
        end
    | LList(e, l), LChi([a], ll) ->
        begin
            match a, ll, e, l with
            | _, [LNil], _, _ -> LPar(lhs, LList(a, LNil))
            | EEta(AOut(j)), _, EEta(AIn(k)), _ -> if k = j then LPar(l, case_e (EEta(AOut(k))) rhs (-1)) else join_chis lhs rhs
            | EEta(AIn(j)), _, EEta(AOut(k)), _ -> if k = j then LPar(l, case_e (EEta(AIn(k))) rhs (-1)) else join_chis lhs rhs
            | _, _, _, _ -> join_chis lhs rhs
        end
    (* Probably only works for LLists now *)
    | LChi(el1, ll1), LChi(el2, ll2) -> 
        let rec calcChi chi =
            match chi with
            | LChi(el3, ll3) -> 
            if exist_corres el3 then (printMode fmt chi; calcChi (case_f chi (-1,-1))) else 
                match chi with
                | LChi([], []) -> LNil
                | LChi(_, _) -> chi
        in calcChi (join_chis lhs rhs)
    | LNil, _ -> rhs
    | _, LNil -> lhs
    | LPar(LNil, LNil), _ -> rhs (* Added just for new_eval *)
    | _, LPar(LNil, LNil) -> lhs (* Added just for new_eval *)
    | LChi(_, _), LPar(_,_) -> join_chis lhs (lparToChi rhs) (* Added just for new_eval/eval_chi *)
    | LPar(_, _), LChi(_,_) -> join_chis (lparToChi lhs) rhs (* Added just for new_eval/eval_chi *)
    | _, _ -> printMode fmt (LPar(lhs,rhs));printf "-> ";raise (RuntimeException "No match in eval_par\n")

let rec sStepEval exp =
    match exp with
    | LNil -> LNil
    | LList(e, l) -> exp
    | LChi(el, ll) -> exp
    | LPar((LPar(_,_) as l1), (LNil as l2))
    | LPar((LPar(_,_) as l1), (LList(_,_) as l2))
    | LPar((LPar(_,_) as l1), (LChi(_,_) as l2)) -> LPar(sStepEval l1, sStepEval l2)
    | LPar(l1, l2) -> let res = eval_par l1 l2 in remLNils res

let rec has_nested_chi exp =
    match exp with
    | LChi(_, _) -> true
    | LPar(LChi(_,_), LNil) | LPar(LNil, LChi(_,_)) 
    | LPar(LChi(_,_), LList(_,_)) | LPar(LList(_,_), LChi(_,_)) 
    | LPar(LChi(_,_), LChi(_,_))-> true 
    | LPar(l1, _) -> has_nested_chi l1
    | _ -> false

let rec can_chi_progress exp =
    match exp with
    | LPar(LChi(el,ll), LList(EEta(AIn(j)), l)) 
    | LPar(LList(EEta(AIn(j)), l), LChi(el, ll)) -> if find_corres_list el (EEta(AOut(j))) 0 != [] || find_all_corres el el 0 0 != [] then true else false
    | LPar(LChi(el,ll), LList(EEta(AOut(j)), l)) 
    | LPar(LList(EEta(AOut(j)), l), LChi(el, ll)) -> if find_corres_list el (EEta(AIn(j))) 0 != [] || find_all_corres el el 0 0 != [] then true else false
    | LChi(el, ll) | LPar(LChi(el, ll), _) | LPar(_, LChi(el, ll)) -> if find_all_corres el el 0 0 != [] then true else false
    | LPar(l1, _) -> can_chi_progress l1
    | _ -> false

let rec can_chi_list_progress exp =
    match exp with
    | LPar(LChi(el, ll), LList(EEta(AIn(j)), l))
    | LPar(LList(EEta(AIn(j)), l), LChi(el, ll)) -> if find_corres_list el (EEta(AOut(j))) 0 != [] then true else false
    | LPar(LChi(el,ll), LList(EEta(AOut(j)), l)) 
    | LPar(LList(EEta(AOut(j)), l), LChi(el, ll)) -> if find_corres_list el (EEta(AIn(j))) 0 != [] then true else false
    | LPar(l1, _) -> can_chi_list_progress l1
    | _ -> false

let rec can_chi_nested_progress exp =
    match exp with
    | LChi(el, ll) | LPar(LChi(el, ll), _) | LPar(_, LChi(el, ll)) -> if find_all_corres el el 0 0 != [] then true else false
    | LPar(l1, _) -> can_chi_nested_progress l1
    | _ -> false


let rec eval_chi exp =
    match exp with
    | (LPar(l1, l2), ctx) ->
        begin
        match l1, l2 with
        | LChi(el, ll), LList(et, l) | LList(et,l), LChi(el,ll) when (List.exists ((=) (compl_eta et)) el) ->
            (*printf "eval_chi case 1: "; *)
            let rec iter il e i =
                (match il with
                | [] -> []
                | hd::tl -> 
                    let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
                    printCtxLevel1 n_ctx.level (List.nth el hd) et hd;
                    printMode fmt (LPar(l1,l2));
                    match l1, l2 with
                    | LChi(_, _), LList(_, _) ->
                        if has_lpar_in_chi l1
                        then ((*printf "Eval_chi case_h: ";*)([[(remLNils (LPar(case_h e l1 hd, l)), n_ctx)]])@(iter tl e (i+1)))
                        else ((*printf "Eval_chi case_e: ";*)([[(remLNils (LPar(case_e e l1 hd, l)), n_ctx)]])@(iter tl e (i+1)))
                    | LList(_, _), LChi(_, _) ->
                        if has_lpar_in_chi l2
                        then ((*printf "Eval_chi case_h: ";*)([[(remLNils (LPar(l, case_h e l2 hd)), n_ctx)]])@(iter tl e (i+1)))
                        else ((*printf "Eval_chi case_e: ";*)([[(remLNils (LPar(l, case_e e l2 hd)), n_ctx)]])@(iter tl e (i+1)))
                    )
            in
            (match et with
            | EEta(AOut(k)) when List.exists((=) (EEta(AIn(k)))) el -> let inds = find_corres_list el (EEta(AIn(k))) 0 in iter inds (EEta(AIn(k))) 1
            | EEta(AIn(k)) when List.exists((=) (EEta(AOut(k)))) el -> let inds = find_corres_list el (EEta(AOut(k))) 0 in iter inds (EEta(AOut(k))) 1
            )
        | LChi(el, ll), _  | _, LChi(el, ll) ->
            let rec iter l i =
                match l with
                | [] -> []
                | hd::tl -> 
                    let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
                    if isLChi l1 && isLChi l2 
                    then printCtxLevel2 n_ctx.level (getEl (join_chis l1 l2)) hd 
                    else printCtxLevel2 n_ctx.level el hd;
                    printMode fmt (LPar(l1,l2));(*printf "Eval_chi case_f: \n"; *)
                    match l1, l2 with
                    | LChi(_, _), LChi(_, _) -> (iter tl (i+1))@([[(case_f (join_chis l1 l2) hd, n_ctx)]]) 
                    | LChi(_,_), _ -> (iter tl (i+1))@([[(remLNils (LPar((case_f l1 hd), l2)), n_ctx)]])
                    | _, LChi(_, _) ->  (iter tl (i+1))@([[(remLNils (LPar(l1, (case_f l2 hd))), n_ctx)]])
            in
            (match l1, l2 with
            | LChi(_,_), LChi(_,_) -> 
                let joined = join_chis l1 l2 in
                    (match joined with
                    | LChi(el1, ll) -> if exist_corres el1 then let corres_l = find_all_corres el1 el1 0 0 in iter corres_l 1 else [[(joined, ctx)]])
            | LChi(_,_),  _ | _, LChi(_,_) when exist_corres el -> let corres_l = find_all_corres el el 0 0 in iter corres_l 1
            | LChi([],[]), LNil | LNil, LChi([], []) -> [[(LNil, ctx)]]
            | (LChi(_,_) as x), LNil | LNil, (LChi(_,_) as x) -> [[(x, ctx)]]
            | LChi(_,_), _ | _, LChi(_,_) -> [[(join_chis l1 l2, ctx)]]
            | _, _ -> print_lambdas fmt (LPar(l1,l2));raise (RuntimeException "No match in eval_chi inside LChi(), _ \n"))
        | (_ as err1), (_ as err2) -> printMode fmt (LPar(err1, err2)); raise (RuntimeException "No match in eval_chi inside LPar")
        end
    | (_ as err, ctx) -> printMode fmt err; raise (RuntimeException "No match in eval_chi")

let rec eval_chi_list exp =
    match exp with
    | (LPar(l1, l2), ctx) ->
        begin
        match l1, l2 with
        | LChi(el, ll), LList(et, l) | LList(et,l), LChi(el,ll) when (List.exists ((=) (compl_eta et)) el) ->
            (*printf "eval_chi case 1: "; *)
            let rec iter il e i =
                (match il with
                | [] -> []
                | hd::tl -> 
                    let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
                    printCtxLevel1 n_ctx.level (List.nth el hd) et hd;
                    printMode fmt (LPar(l1,l2));
                    match l1, l2 with
                    | LChi(_, _), LList(_, _) ->
                        if has_lpar_in_chi l1
                        then ((*printf "Eval_chi case_h: ";*)([[(remLNils (LPar(case_h e l1 hd, l)), n_ctx)]])@(iter tl e (i+1)))
                        else ((*printf "Eval_chi case_e: ";*)([[(remLNils (LPar(case_e e l1 hd, l)), n_ctx)]])@(iter tl e (i+1)))
                    | LList(_, _), LChi(_, _) ->
                        if has_lpar_in_chi l2
                        then ((*printf "Eval_chi case_h: ";*)([[(remLNils (LPar(l, case_h e l2 hd)), n_ctx)]])@(iter tl e (i+1)))
                        else ((*printf "Eval_chi case_e: ";*)([[(remLNils (LPar(l, case_e e l2 hd)), n_ctx)]])@(iter tl e (i+1)))
                    )
            in
            (match et with
            | EEta(AOut(k)) when List.exists((=) (EEta(AIn(k)))) el -> let inds = find_corres_list el (EEta(AIn(k))) 0 in iter inds (EEta(AIn(k))) 1
            | EEta(AIn(k)) when List.exists((=) (EEta(AOut(k)))) el -> let inds = find_corres_list el (EEta(AOut(k))) 0 in iter inds (EEta(AOut(k))) 1
            )
        end

let rec eval_chi_nested exp =
    match exp with
    | (LPar(l1, l2), ctx) ->
        begin
        match l1, l2 with
        | LChi(el, ll), _  | _, LChi(el, ll) ->
            let rec iter l i =
                match l with
                | [] -> []
                | hd::tl -> 
                    let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
                    if isLChi l1 && isLChi l2 
                    then printCtxLevel2 n_ctx.level (getEl (join_chis l1 l2)) hd 
                    else printCtxLevel2 n_ctx.level el hd;
                    printMode fmt (LPar(l1,l2));(*printf "Eval_chi case_f: \n"; *)
                    match l1, l2 with
                    | LChi(_, _), LChi(_, _) -> (iter tl (i+1))@([[(case_f (join_chis l1 l2) hd, n_ctx)]]) 
                    | LChi(_,_), _ -> (iter tl (i+1))@([[(remLNils (LPar((case_f l1 hd), l2)), n_ctx)]])
                    | _, LChi(_, _) ->  (iter tl (i+1))@([[(remLNils (LPar(l1, (case_f l2 hd))), n_ctx)]])
            in
            (match l1, l2 with
            | LChi(_,_), LChi(_,_) -> 
                let joined = join_chis l1 l2 in
                    (match joined with
                    | LChi(el1, ll) -> if exist_corres el1 then let corres_l = find_all_corres el1 el1 0 0 in iter corres_l 1 else [[(joined, ctx)]])
            | LChi(_,_),  _ | _, LChi(_,_) when exist_corres el -> let corres_l = find_all_corres el el 0 0 in iter corres_l 1
            | _, _ -> print_lambdas fmt (LPar(l1,l2));raise (RuntimeException "No match in eval_chi inside LChi(), _ \n"))
        end

let rec eval arr =
    let rec new_eval expInArr =
        match expInArr with
        | [] -> []
        | hd::tl ->
            begin
            match hd with
            | (LPar(l1, l2) as lp, ctx) ->
                (*printf "Entrei new_eval\n";*)
                let ass_lp = assocLeft lp in
                if has_nested_chi ass_lp = false
                then (
                    printCtxLevel ctx.level;
                    printMode fmt ass_lp;
                    let oneEval = sStepEval ass_lp in
                    printMode fmt oneEval;
                    if getParNum oneEval <= 2
                    then ((*printf"menos que 2: %d\n" (getParNum oneEval);*)(new_eval tl)@(new_eval [(oneEval, next_ctx ctx)]))
                    else ((*printf"mais que 2\n";*)let toList = lparToList oneEval in
                    let combs = List.flatten (topComb toList) in
                    let i = ref 0 in
                    let combs_ctx = List.map (fun x -> i:=!i+1; (x, {ctx with level = ctx.level ^ "." ^string_of_int !i})) combs in
                    (new_eval tl)@(new_eval combs_ctx))
                ) else (
                    printCtxLevel ctx.level; 
                    printMode fmt ass_lp;
                    let chi_l_prog = can_chi_list_progress ass_lp in
                    let chi_n_prog = can_chi_nested_progress ass_lp in
                    match chi_l_prog, chi_n_prog with
                    | true, true ->
                        let chi_nested = List.flatten (eval_chi_nested (getNestedLeft ass_lp, ctx)) in
                        let add_after1 = if getParNum ass_lp <= 2 then chi_nested else List.map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_nested in
                        let chi_list = List.flatten ( eval_chi_list (getNestedLeft ass_lp, ctx)) in
                        let add_after2 = if getParNum ass_lp <= 2 then chi_list else List.map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_list in
                        List.flatten (
                        List.map (
                            fun x ->
                            match x with
                            | (lp, ctx) ->
                                if getParNum lp <= 2 then (new_eval tl)@(new_eval [x]) else
                                let toList = lparToList lp in
                                let combs = List.flatten (topComb toList) in
                                let i = ref 0 in
                                let combs_pairs = List.map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                                (new_eval tl)@(new_eval combs_pairs)
                        ) (add_after1@add_after2)
                        )
                    | true, false ->
                        let chi_list = List.flatten ( eval_chi_list (getNestedLeft ass_lp, ctx)) in
                        let add_after2 = if getParNum ass_lp <= 2 then chi_list else List.map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_list in
                        List.flatten (
                        List.map (
                            fun x ->
                            match x with
                            | (lp, ctx) ->
                                if getParNum lp <= 2 then (new_eval tl)@(new_eval [x]) else
                                let toList = lparToList lp in
                                let combs = List.flatten (topComb toList) in
                                let i = ref 0 in
                                let combs_pairs = List.map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                                (new_eval tl)@(new_eval combs_pairs)
                        ) (add_after2)
                        )
                    | false, true ->
                        let chi_nested = List.flatten (eval_chi_nested (getNestedLeft ass_lp, ctx)) in
                        let add_after1 = if getParNum ass_lp <= 2 then chi_nested else List.map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_nested in
                        List.flatten (
                        List.map (
                            fun x ->
                            match x with
                            | (lp, ctx) ->
                                if getParNum lp <= 2 then (new_eval tl)@(new_eval [x]) else
                                let toList = lparToList lp in
                                let combs = List.flatten (topComb toList) in
                                let i = ref 0 in
                                let combs_pairs = List.map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                                (new_eval tl)@(new_eval combs_pairs)
                        ) (add_after1)
                        )                            
                    | false, false ->
                        let n_left = getNestedLeft ass_lp in
                        match n_left with
                        | LPar(LChi([], []), LNil) | LPar(LNil, LChi([], [])) -> 
                            if getParNum ass_lp <= 2 
                            then (new_eval tl)@[[(LNil, ctx)]] 
                            else (new_eval tl)@(new_eval [addAfterChiEval2 (LNil, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
                        | LPar((LChi(_,_) as l1), (LList(_,_) as l2)) | LPar((LList(_,_) as l1), (LChi(_,_) as l2))
                        | LPar((LChi(_,_) as l1), (LChi(_,_) as l2)) -> 
                            if getParNum ass_lp <= 2 
                            then (new_eval tl)@(new_eval [(join_chis l1 l2, ctx)])
                            else (new_eval tl)@(new_eval [addAfterChiEval2 (join_chis l1 l2, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
                        | LPar(LNil, (LChi(_,_) as l1)) | LPar((LChi(_,_) as l1), LNil) ->
                            if getParNum ass_lp <= 2
                            then (new_eval tl)@(new_eval [(l1, ctx)])
                            else (new_eval tl)@(new_eval [addAfterChiEval2 (l1, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
                        | _ -> printMode fmt ass_lp; raise (RuntimeException "No match in new_eval_1")
            ) 
            | (LChi([], []), ctx) -> printCtxLevel ctx.level; printMode fmt (LNil); (new_eval tl)@[[(LNil, ctx)]]
            | (LChi(el, ll) as a, ctx) when exist_corres el -> (new_eval tl)@(new_eval (List.flatten (eval_chi_nested (LPar(a,LNil), ctx))))
            | (LNil, ctx) -> (new_eval tl)@[[(LNil, ctx)]]
            | (_ as err, ctx) -> printMode fmt err; raise (RuntimeException "No match in new_eval")
            end
        in 
        match arr with
        | [] -> []
        | hd::tl -> (eval tl)@(new_eval hd)


    
let main exp =
    let lamExp = toLambda exp in
    let toList = lparToList lamExp  in
    let res = if List.length toList <= 2 
              then (eval [[(lamExp, {print=true; level="1"})]])
              else let comb_lst = topComb toList in 
              printFinalArrComb fmt (List.flatten comb_lst); eval (assign_ctx2 comb_lst)
    in
    let findings = proc_findings_comb (List.flatten res) in
    if List.length findings = List.length res
    then printf "\nThe process has a deadlock: every process combination is blocked.\n"
    else if List.length findings = 0 
        then printf "\nThe process is deadlock-free.\n"
        else print_findings_comb (List.rev findings);
    if !verbose then let _ = printf "\n" in print_list_comb fmt (List.rev (List.flatten res)) else ()
;;

(* ------------------- TESTING -------------------- *)

(* main ( PPar( PPar(PPref(AIn('a'), PPar(PPref(AOut('b'), PNil) , PPref(AIn('c'), PPref(AIn('d'), PNil)))) , PPref(AIn('b'), PNil) ) , PPref(AOut('a'), PPar(PPref(AOut('c'), PNil), PPref(AOut('d'), PNil)))) ) *)
(* main ( PPar(PPar(PPref(AOut('b'), PPar(PNil, PNil)), PPref(AIn('b'), PNil) ), PPref(AIn('b'), PPref(AOut('b'), PPar(PPref(AOut('b'), PNil) , PPref(AIn('b'), PNil))))) ) *)

main ( PPar( PPar(PPref(AIn('a'), PPar(PPref(AOut('b'), PNil) , PPref(AIn('c'), PPref(AIn('d'), PNil)))) , PPref(AIn('b'), PNil) ) , PPref(AOut('a'), PPar(PPref(AOut('c'), PNil), PPref(AOut('d'), PNil)))) )

(* main ( PPar(PPar(PPref(AIn('a'), PNil), PPref(AIn('a'), PNil)), PPref(AOut('a'), PPar(PPref(AOut('a'), PNil), PPref(AIn('b'), PNil))) ) ) *)
