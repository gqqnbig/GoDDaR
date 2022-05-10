open Format
open Dlock
open Dlock.Auxfunctions
open Dlock.Main_act_verifier_orig
open Dlock.Types
open Dlock.Types.Eta
open Dlock.Printer
open Printer2
open Cmd


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
let rec reduceChi (LChi(hd::tl, hd1::tl1) as chi) i_at i =
  if i = i_at then
    (LChi(tl, tl1))
  else
    join_chis (LChi([hd],[hd1])) (reduceChi (LChi(tl, tl1)) i_at (i+1))

(* Removes the EEta from a LChi's LList at index i_at *)
let rec correct_chi_lambda ll curr_i i_at =
  match ll with
  | [] -> raise (RuntimeException "correct_chi_lambda failed: Empty lambda list")
  | hd::tl ->
    if curr_i = i_at then
      (match hd with
      | LList(EEta(_), LList(EEta(a), l)) -> LList(EEta(a), l)::tl
      | LList(EEta(_), LNil) -> LNil::tl
      | LList(EEta(_), LPar(m1, m2)) -> LPar(m1,m2)::tl
      | LList(EEta(_), LOr(m1,m2)) -> LOr(m1,m2)::tl
      | LNil -> tl
      | _ -> print_lambdas fmt hd ;raise (RuntimeException "correct_chi_lambda failed: No match "))
    else
      hd::(correct_chi_lambda tl (curr_i+1) i_at)

(* Similar to correct_chi_lambda, but returns EEta at index i_at instead of removing it *)
let rec find_chi_lambda (LChi(et, hd::tl)) curr_i i_at =
  if curr_i = i_at then
    (match hd with
    | LList(a, LList(b, l)) -> a
    | LList(a, LNil) -> a
    | LList(a, LPar(_, _)) -> a
    | LList(a, LOr(_,_)) -> a
    | _ -> print_lambdas fmt hd ;raise (RuntimeException "find_chi_lambda failed: Unexpected match")) (* Pode ser preciso para o 3º caso*)
  else
    find_chi_lambda (LChi(et, tl)) (curr_i+1) i_at

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
  let f_list = List.filteri (fun i _ -> i = i_at ) list in (List.hd f_list)

(* Pulls the next Eta and arranges Chi's ll*)
let case_e elem (LChi(el, ll) as chi) i_at =
  let at_index = if i_at = -1 then find el elem else i_at in
  let nth_elem = List.nth el at_index in
  let nth_ll = List.nth ll at_index in
    match nth_ll with
    | LNil -> LChi(List.filteri (fun i _ -> i!=at_index) el, List.filteri (fun i _ -> i!=at_index) ll) 
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
let case_f (LChi(el, ll) as chi) i_pair =
  let (a, b) = if i_pair = (-1,-1) then find_corres el el 0 0 else i_pair in
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
    | LNil, _ -> let LChi(el1, ll1) as r_chi = reduceChi chi a 0 in
                  let chi_lambdaB = find_chi_lambda r_chi 0 (b-1) in
                    (LChi(subst_at el1 chi_lambdaB 0 (b-1), correct_chi_lambda ll1 0 (b-1)))
    | LPar(_,_), LNil -> let r_chi = reduceChi chi b 0 in
                         let lpChi = lparToChi (get_chi_at ll a) in
                         let rr_chi = reduceChi r_chi a 0 in
                            join_chis rr_chi lpChi
    | _, LNil -> let LChi(el1, ll1) as r_chi = reduceChi chi b 0 in
                  let chi_lambdaA = find_chi_lambda r_chi 0 a in
                    (LChi(subst_at el1 chi_lambdaA 0 a, correct_chi_lambda ll1 0 a))
    )
  else
    let chi_lambdaA = find_chi_lambda chi 0 a in
    let chi_lambdaB = find_chi_lambda chi 0 b in
    LChi(subst_first (subst_first el elemA_el chi_lambdaA) elemB_el chi_lambdaB , correct_chi_lambda (correct_chi_lambda ll 0 a) 0 b)


(* Checks whether there are corresponding actions in list *)
let rec exist_corres list =
  match list with
  | [] -> false
  | (EEta(_) as eta)::tl  -> List.exists ((=) (compl_eta eta)) tl || exist_corres tl

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LChi at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_chi elem (LChi(el, ll)) =
  let i_at = find el elem in
      if i_at = -1 then
        false
      else
        let res = List.filteri(
            fun i a -> 
                if i = i_at then 
                    match a with
                    | LChi(_, _) -> true
                    | _ -> false
                else false) ll
          in 
            List.length res = 1

(* Defines the case when there are two correspondent actions and a Chi must be pulled from the level below *)
(* Example case: a?0 x (b! | a!; 0, (c! | d?; 0, 0)) -> 0 x (b! | c! | d?; 0; 0; 0) *)
let case_g elem (LChi(el, ll)) =
  let i_at = find el elem in
  let f_el = List.filteri ( fun i _ -> i!=i_at ) el in
  let f_ll = List.filteri ( fun i _ -> i!=i_at ) ll in
    join_chis (LChi(f_el, f_ll)) (get_chi_at ll i_at)

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LPar at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_par elem (LChi(el, ll)) =
  let i_at = find el elem in
  if i_at = -1 then
    false
  else
    let res = List.filteri(
        fun i a -> 
            if i = i_at then 
                match a with
                | LPar(_, _) -> true
                | _ -> false
            else
              false) ll
    in 
      List.length res = 1

(* For the case where a parallel composition is prefixed by some other action *)
(* Example case: a?.0 x (a! | b?; (b!.0 || c?.0); 0) -> (b! || c? || b?; 0; 0; 0) *)
let case_h elem (LChi(el, ll)) at_i =
  let i_at = if at_i = -1 then find el elem else at_i in
  let f_el = List.filteri ( fun i _ -> i!=i_at ) el in
  let f_ll = List.filteri ( fun i _ -> i!=i_at ) ll in
    join_chis (LChi(f_el, f_ll)) (lparToChi (get_chi_at ll i_at))

(* For the case where a choice is nested in a Chi's ll *)
let rec case_i chi i_at =
  match chi with
  | LChi(el, ll) ->
      let l_or = List.nth ll i_at in
      let rec treat_or (LChi(el, ll) as chi) i_at n_or =
        match n_or with
        | LOr(LOr(_, _) as x, LList(a, b)) -> (treat_or chi i_at x)@[LChi(subst_at el a 0 i_at, subst_at ll b 0 i_at)]
        | LOr(LOr(_, _) as x, LNil) -> (treat_or chi i_at x)@[LChi(el, subst_at ll (LNil) 0 i_at)]
        | LOr(LList(a, b), LList(c, d)) -> [LChi(subst_at el a 0 i_at, subst_at ll b 0 i_at); LChi(subst_at el c 0 i_at, subst_at ll d 0 i_at)]
        | LOr(LList(a, b), LNil) -> [LChi(subst_at el a 0 i_at, subst_at ll b 0 i_at); LChi(el, subst_at ll (LNil) 0 i_at)]
        | LOr(LNil, LList(a, b)) -> [LChi(el, subst_at ll (LNil) 0 i_at); LChi(subst_at el a 0 i_at, subst_at ll b 0 i_at)]
        | LOr(LNil, LNil) -> [LChi(el, subst_at ll (LNil) 0 i_at)]
        | _ -> print_lambdas fmt n_or; raise (RuntimeException "No match in treat_or inside case_i\n")
      in treat_or chi i_at l_or
  | _ -> print_lambdas fmt chi; raise (RuntimeException "No match in case_i\n")

let case_f_or (LChi(el, ll) as chi) ((a, b) as i_pair) =
  let elemA_el = List.nth el a in 
  let elemB_el = List.nth el b in
  let elemA_ll = List.nth ll a in
  let elemB_ll = List.nth ll b in
  match elemA_ll, elemB_ll with
  | LOr(_, _), LNil -> let r_chi = reduceChi chi b 0 in case_i r_chi a
  | LNil, LOr(_, _) -> let r_chi = reduceChi chi a 0 in case_i r_chi (b-1)
  | LOr(_, _), LList(_, _) -> let i_chi = case_i chi a in map (fun x -> case_e (List.nth el b) x b) i_chi
  | LList(_, _), LOr(_, _) -> let i_chi = case_i chi b in map (fun x -> case_e (List.nth el a) x a) i_chi
  | LOr(_, _), LOr(_, _) -> let n_chi = case_i chi b in flatten2( map (fun x -> case_i x a) n_chi )
  | _ -> printMode fmt elemA_ll true; printMode fmt elemB_ll true; raise (RuntimeException "No match in case_f_or\n")


let rec has_lpar_in_chi chi =
  match chi with
  | LChi([], []) -> false
  | LChi(ehd::etl, lhd::ltl) ->
      match lhd with
      | LPar(_, _) -> true
      | _ -> has_lpar_in_chi (LChi(etl, ltl))

let eval_par lhs rhs =
  match lhs, rhs with
  | LList(EEta(a),l1), LList(EEta(b),l2) ->
      begin
          match a, b with
          | AIn(k), AOut(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B*)
          | AOut(k), AIn(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B *)
          | _, _ -> LChi([EEta(a);EEta(b)], l1::l2::[])                                            (* Case B *)
      end
  | LNil, _ -> rhs
  | _, LNil -> lhs
  | LPar(LNil, LNil), _ -> rhs (* Added just for new_eval *)
  | _, LPar(LNil, LNil) -> lhs (* Added just for new_eval *)
  | _, _ -> printMode fmt (LPar(lhs,rhs)) true;printf "-> ";raise (RuntimeException "No match in eval_par\n")

let rec sStepEval exp =
  match exp with
  | LNil -> LNil
  | LList(e, l) -> exp
  | LChi(el, ll) -> exp
  | LOr(_, _) -> exp
  | LPar((LPar(_,_) as l1), (LNil as l2))
  | LPar((LPar(_,_) as l1), (LList(_,_) as l2))
  | LPar((LPar(_,_) as l1), (LChi(_,_) as l2)) 
  | LPar((LPar(_,_) as l1), (LOr(_,_) as l2)) -> LPar(sStepEval l1, sStepEval l2)
  | LPar(l1, l2) -> let res = eval_par l1 l2 in remLNils res

let rec eval_chi_list ((LPar(l1, l2), ctx) as exp) =
  match l1, l2 with
  | LChi(el, ll), LList(et, l)
  | LList(et,l), LChi(el,ll) when (List.exists ((=) (compl_eta et)) el) ->
    let rec iter il e i =
      (match il with
      | [] -> []
      | hd::tl -> 
          let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
          printCtxLevel1 n_ctx (List.nth el hd) et hd;
          printMode fmt (LPar(l1,l2)) n_ctx.print;
          match l1, l2 with
          | LChi(_, _), LList(_, _) ->
              if has_lpar_in_chi l1 && (isLPar (List.nth ll hd)) then
                (([[(remLNils (LPar(case_h e l1 hd, l)), n_ctx)]])@(iter tl e (i+1)))
              else
                  if isLOr (List.nth ll hd) then 
                    let case_i = case_i l1 hd in
                      [List.mapi ( fun j x -> (remLNils (LPar(x, l)), conc_lvl n_ctx (string_of_int (j+1)))) case_i] 
                  else
                    ([[(remLNils (LPar(case_e e l1 hd, l)), n_ctx)]])@(iter tl e (i+1))
          | LList(_, _), LChi(_, _) ->
              if has_lpar_in_chi l2 && (isLPar (List.nth ll hd)) then
                (([[(remLNils (LPar(l, case_h e l2 hd)), n_ctx)]])@(iter tl e (i+1)))
              else
                  if isLOr (List.nth ll hd) then 
                    let case_i = case_i l2 hd in
                        [List.mapi ( fun j x -> (remLNils (LPar(l, x)), conc_lvl n_ctx (string_of_int (j+1))) ) case_i]
                  else
                    ([[(remLNils (LPar(l, case_e e l2 hd)), n_ctx)]])@(iter tl e (i+1))
      )
    in
      (
        match et with
      | EEta(AOut(k)) when List.exists((=) (EEta(AIn(k)))) el ->
        let inds = find_corres_list el (EEta(AIn(k))) 0 in
        iter inds (EEta(AIn(k))) 1
      | EEta(AIn(k)) when List.exists((=) (EEta(AOut(k)))) el ->
        let inds = find_corres_list el (EEta(AOut(k))) 0 in
        iter inds (EEta(AOut(k))) 1
      )

let rec eval_chi_nested ((LPar(l1, l2), ctx) as exp) =
  match l1, l2 with
  | LChi(el, ll), _
  | _, LChi(el, ll) ->
    let rec iter l i =
      match l with
      | [] -> []
      | hd::tl -> 
        let n_ctx = {ctx with level = ctx.level ^ "." ^ string_of_int i} in 
        if isLChi l1 && isLChi l2 then
          printCtxLevel2 n_ctx (getEl (join_chis l1 l2)) hd 
        else
          printCtxLevel2 n_ctx el hd;
          printMode fmt (LPar(l1,l2)) n_ctx.print;
          match l1, l2 with
          | LChi(_, _), LChi(_, _) ->
            let n_chi = join_chis l1 l2 in
            (match n_chi with
            | LChi(el1, ll1) ->
              if has_lor_at ll1 hd then
                let cf_res = case_f_or n_chi hd in
                (iter tl (i+1))@[List.mapi ( fun k x -> (x, conc_lvl ctx (string_of_int (k+1))) ) cf_res]
              else
                (iter tl (i+1))@([[(case_f n_chi hd, n_ctx)]])) 
          | LChi(_,_), _ -> 
            if has_lor_at ll hd then
              let cf_res = case_f_or l1 hd in
              (iter tl (i+1))@[List.mapi ( fun k x -> (remLNils (LPar(x, l2)), conc_lvl ctx (string_of_int (k+1)))) cf_res]
            else
              (iter tl (i+1))@([[(remLNils (LPar((case_f l1 hd), l2)), n_ctx)]])
          | _, LChi(_, _) ->
            if has_lor_at ll hd then
              let cf_res = case_f_or l2 hd in
              (iter tl (i+1))@[List.mapi ( fun k x -> (remLNils (LPar(l1, x)), conc_lvl ctx (string_of_int (k+1)))) cf_res]
            else
              (iter tl (i+1))@([[(remLNils (LPar(l1, (case_f l2 hd))), n_ctx)]])
    in
      (match l1, l2 with
      | LChi(_,_), LChi(_,_) -> 
        let (LChi(el1, ll) as joined) = join_chis l1 l2 in
          if exist_corres el1 then
            let corres_l = find_all_corres el1 el1 0 0 in
              iter corres_l 1
          else
            [[(joined, ctx)]]
      | LChi(_,_),  _ | _, LChi(_,_) when exist_corres el ->
        let corres_l = find_all_corres el el 0 0 in
          iter corres_l 1
      | _, _ -> print_lambdas fmt (LPar(l1,l2));raise (RuntimeException "No match in eval_chi inside LChi(), _ \n"))

let eval arr =
  let rec new_eval expInArr =
  match expInArr with
  | [] -> []
  | ((l, ctx) as hd)::tl ->
    begin
    printCtxLevel ctx;
    match l with
    | LOr(l1, l2) ->
      let ass_lo = assocLeft l in
      printMode fmt ass_lo ctx.print;
      let ctx_l = conc_lvl ctx "1" in
      let ctx_r = conc_lvl ctx "2" in
        append (new_eval tl) (new_eval [(getL1 ass_lo, ctx_l); (getL2 ass_lo, ctx_r)])
    | LPar(l1, l2) ->
      let ass_lp = assocLeft l in
      if has_nested_or ass_lp then (
        printMode fmt ass_lp ctx.print;
        let ctx_l = conc_lvl ctx "1" in
        let ctx_r = conc_lvl ctx "2" in
        match getNestedLeft ass_lp with
        | LPar(LOr(a,b), c) -> 
          if getParNum ass_lp <=2 then
            append (new_eval tl) (new_eval [(LPar(a, c), ctx_l); (LPar(b,c), ctx_r)])
          else 
            let add_after_l = addAfterChiEval2 (LPar(a,c), ctx_l) (assocLeftList (getRestPars (lparToList ass_lp))) in
            let add_after_r = addAfterChiEval2 (LPar(b,c), ctx_r) (assocLeftList (getRestPars (lparToList ass_lp))) in
              append (new_eval tl) (new_eval [add_after_l; add_after_r])
        | LPar(a, LOr(b, c)) -> 
          if getParNum ass_lp <=2 then
            append (new_eval tl) (new_eval [(LPar(a, b), ctx_l); (LPar(a, c), ctx_r)])
          else
            let add_after_l = addAfterChiEval2 (LPar(a,b), ctx_l) (assocLeftList (getRestPars (lparToList ass_lp))) in
            let add_after_r = addAfterChiEval2 (LPar(a,c), ctx_r) (assocLeftList (getRestPars (lparToList ass_lp))) in
              append (new_eval tl) (new_eval [add_after_l; add_after_r])
        | _ -> printMode fmt (getNestedLeft ass_lp) true; raise (RuntimeException "No match in new_eval OR")
      ) else
        if has_nested_chi ass_lp = false then (
          printMode fmt ass_lp ctx.print;
          let oneEval = sStepEval ass_lp in
          if getParNum oneEval <= 2 then
            append (new_eval tl) (new_eval [(oneEval, next_ctx ctx)])
          else (
            let toList = lparToList oneEval in
            let combs = flatten2 (topComb toList) in
            let i = ref 0 in
            let combs_ctx = map (fun x -> i:=!i+1; (x, {ctx with level = ctx.level ^ "." ^string_of_int !i})) combs in
              append (new_eval tl) (new_eval combs_ctx)
          )
        ) else (
          printMode fmt ass_lp ctx.print;
          let chi_l_prog = can_chi_list_progress ass_lp in
          let chi_n_prog = can_chi_nested_progress ass_lp in
          match chi_l_prog, chi_n_prog with
          | true, true ->
            (* printf "ENTREI TRUE TRUE\n";*)
            let chi_nested = flatten2 (eval_chi_nested (getNestedLeft ass_lp, ctx)) in
            let add_after1 = if getParNum ass_lp <= 2 then chi_nested else map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_nested in
            let chi_list = flatten2 ( eval_chi_list (getNestedLeft ass_lp, ctx)) in
            let add_after2 = if getParNum ass_lp <= 2 then chi_list else map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_list in
              flatten2 (
                map (
                  fun ((lp, ctx) as x) ->
                    if getParNum lp <= 2 then append (new_eval tl) (new_eval [x]) else
                    let toList = lparToList lp in
                    let combs = flatten2 (topComb toList) in
                    let i = ref 0 in
                    let combs_pairs = map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                      append (new_eval tl) (new_eval combs_pairs)
                )
                (append add_after1 add_after2)
              )
          | true, false ->
            let chi_list = flatten2 ( eval_chi_list (getNestedLeft ass_lp, ctx)) in
            let add_after2 = if getParNum ass_lp <= 2 then chi_list else map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_list in
              flatten2 (
                map (
                  fun ((lp, ctx) as x) ->
                    if getParNum lp <= 2 then
                      append (new_eval tl) (new_eval [x])
                    else
                      let toList = lparToList lp in
                      let combs = flatten2 (topComb toList) in
                      let i = ref 0 in
                      let combs_pairs = map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                        append (new_eval tl) (new_eval combs_pairs)
                ) (add_after2)
              )
          | false, true ->
            let chi_nested = flatten2 (eval_chi_nested (getNestedLeft ass_lp, ctx)) in
            let add_after1 = if getParNum ass_lp <= 2 then chi_nested else map ( fun x -> addAfterChiEval2 x (assocLeftList (getRestPars (lparToList ass_lp))) ) chi_nested in
              flatten2 (
                map (
                  fun ((lp, ctx) as x) ->
                    if getParNum lp <= 2 then
                      append (new_eval tl) (new_eval [x])
                    else
                      let toList = lparToList lp in
                      let combs = flatten2 (topComb toList) in
                      let i = ref 0 in
                      let combs_pairs = map (fun y -> i:=!i+1; (y, {ctx with level = ctx.level ^ "." ^ string_of_int !i})) combs in
                        append (new_eval tl) (new_eval combs_pairs)
                )
                (add_after1)
              )
          | false, false ->
            let n_left = getNestedLeft ass_lp in
            match n_left with
            | LPar(LChi([], []), LNil) | LPar(LNil, LChi([], [])) -> 
              if getParNum ass_lp <= 2 then
                append (new_eval tl) [[(LNil, ctx)]] 
              else
                append (new_eval tl) (new_eval [addAfterChiEval2 (LNil, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
            | LPar((LChi(_,_) as l1), (LList(_,_) as l2)) | LPar((LList(_,_) as l1), (LChi(_,_) as l2))
            | LPar((LChi(_,_) as l1), (LChi(_,_) as l2)) -> 
              if getParNum ass_lp <= 2 then
                append (new_eval tl) (new_eval [(join_chis l1 l2, ctx)])
              else
                append (new_eval tl) (new_eval [addAfterChiEval2 (join_chis l1 l2, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
            | LPar(LNil, (LChi(_,_) as l1)) | LPar((LChi(_,_) as l1), LNil) ->
              if getParNum ass_lp <= 2 then
                append (new_eval tl) (new_eval [(l1, ctx)])
              else
                append (new_eval tl) (new_eval [addAfterChiEval2 (l1, ctx) (assocLeftList (getRestPars (lparToList ass_lp)))])
            | _ -> printMode fmt ass_lp true; raise (RuntimeException "No match in new_eval_1")
        )
    | LChi([], []) ->
      printMode fmt (LNil) ctx.print;
      append (new_eval tl) [[(LNil, ctx)]]
    | LChi(el, ll) when exist_corres el -> 
      printMode fmt (LPar(l,LNil)) ctx.print; 
      append (new_eval tl) (new_eval (flatten2 (eval_chi_nested (LPar(l,LNil), ctx))))
    | LChi([a], [b]) ->
      let p = LList(a, b) in
        (printMode fmt p ctx.print;
        append (new_eval tl) [[(p, ctx)]]) (* Adicionado por causa da main *)
    | LChi(_,_) ->
      printMode fmt l ctx.print;
      append (new_eval tl) [[hd]]
    | LList(et, ll) ->
      printMode fmt l ctx.print;
      append (new_eval tl) [[hd]]
    | LNil ->
      printMode fmt (LNil) ctx.print;
      append (new_eval tl) [[(LNil, ctx)]]
    | _ as err ->
      printMode fmt err true;
      raise (RuntimeException "No match in new_eval")
    end
  in 
    match arr with
    | [] -> []
    | hd::tl -> List.fold_left (fun acc x -> append (new_eval x) acc) [[]] arr

let rec top_lvl_extractor exp =
  match exp with
  | LPar(LNil, LNil) -> []
  | LPar(LList(a, _), LList(b, _)) -> [a;b]
  | LPar(LList(a, _), LNil)
  | LPar(LNil, LList(a, _)) -> [a]
  | LPar(LChi(a, _), LList(b, _))
  | LPar(LList(b,_), LChi(a, _)) -> append a [b]
  | LPar(LChi(a, _), LChi(b, _)) -> append a b
  | LPar(LChi(a, _), LNil)
  | LPar(LNil, LChi(a, _))-> a
  | LPar(LPar(_,_) as a, LList(b, _)) -> b::(top_lvl_extractor a)
  | LPar(LPar(_, _) as a, LChi(b, _)) -> append b (top_lvl_extractor a)
  | LPar(LPar(_, _) as a, LNil) -> top_lvl_extractor a
  | LList(a, _) -> [a]
  | LChi(a, _) -> a
  | _ as err -> printMode fmt err true; raise (RuntimeException "No match in top_lvl_extractor \n")

let rec get_top_lvl arr =
  match arr with
  | [] -> []
  | hd::tl -> (top_lvl_extractor hd)::(get_top_lvl tl)

let rec deadlock_solver_1 exp top_lvl =
  match exp with
  | LPar(a , b) -> LPar(deadlock_solver_1 a top_lvl, deadlock_solver_1 b top_lvl)
  | LOr(a, b) -> LOr(deadlock_solver_1 a top_lvl, deadlock_solver_1 b top_lvl)
  | LNil -> LNil
  | LList(EEta(_) as a, b) -> if List.mem a top_lvl then LPar(LList(a, LNil), b) else LList(a, deadlock_solver_1 b top_lvl)
  | LChi(a, b) -> 
    let rec for_all e_arr l_arr =
      match e_arr, l_arr with
      | [a], [b] -> if List.mem a top_lvl then LPar(LList(a, LNil), b) else LList(a, deadlock_solver_1 b top_lvl)
      | a::e_tl, b::l_tl -> if List.mem a top_lvl then LPar(LPar(LList(a, LNil), b), for_all e_tl l_tl) else LPar(LList(a, deadlock_solver_1 b top_lvl), for_all e_tl l_tl)
    in for_all a b

let rec deadlock_solver_2 exp top_lvl =
  match exp with
  | LPar(a, b) -> LPar(deadlock_solver_2 a top_lvl, deadlock_solver_2 b top_lvl)
  | LOr(a, b) -> LOr(deadlock_solver_2 a top_lvl, deadlock_solver_2 b top_lvl)
  | LNil -> LNil
  | LList(EEta(AOut(_)) as a, b) when List.mem a top_lvl -> LPar(LList(a, LNil), deadlock_solver_2 b top_lvl)
  | LList(EEta(AOut(k)), b) when List.mem (EEta(AIn(k))) top_lvl -> deadlock_solver_2 b top_lvl
  | LList(EEta(AOut(_)) as a, b) -> LList(a, deadlock_solver_2 b top_lvl)
  | LList(EEta(AIn(k)) as a, b) when List.mem a top_lvl -> LPar(LList(a, deadlock_solver_2 b top_lvl), LList(EEta(AOut(k)), LNil))
  | LList(EEta(AIn(_)) as a, b) -> LList(a, deadlock_solver_2 b top_lvl)
  | LChi(a, b) ->
    let rec for_all e_arr l_arr =
      match e_arr, l_arr with
      | [x], [y] -> deadlock_solver_2 (LList(x, y)) top_lvl
      | x::e_tl, y::l_ltl -> LPar(deadlock_solver_2 (LList(x,y)) top_lvl, for_all e_tl l_ltl)
    in for_all a b

let main_deadlock_solver arr hd_only =
  let p = if hd_only then [List.hd arr] else arr in
  let top_lvl = get_top_lvl p in
  let combined = List.combine p top_lvl in
    List.rev (
      map ( fun (a, b) -> 
        if (!ds = 0 || !ds = 1) then
          deadlock_solver_1 a b
        else
          deadlock_solver_2 a b
      ) combined 
    )

let rec find_deadl_exp exp dexp =
  if exp = dexp then
    LSubst
  else
    match exp with
    | LPar(a, b) -> if a = dexp then LPar(LSubst, b) else if b = dexp then LPar(a, LSubst) else LPar(find_deadl_exp a dexp, find_deadl_exp b dexp)
    | LOr(a, b) -> if a = dexp then LOr(LSubst, b) else if b = dexp then LOr(a, LSubst) else LOr(find_deadl_exp a dexp, find_deadl_exp b dexp)
    | LList(a, b) -> if b = dexp then LList(a, LSubst) else LList(a, find_deadl_exp b dexp)
    | LNil -> LNil
    | _ -> raise ( RuntimeException "No match")


let rec det_res_loop arr =
match arr with
| [] -> []
| hd::tl ->
  let toList1 = lparToList hd in
  let res1 = (eval [[((lchi_to_lpar hd), {print=false; level="a"})]]) in
  let flat_res1 = flatten2 res1 in
  let findings = proc_findings_comb flat_res1 in
  if List.length findings != 0 then
    let result = List.hd (List.filter (fun x -> x <> LNil) (fst flat_res1)) in
    let deadl_exp = find_deadl_exp hd result in
    let use_subst = use_lsubst deadl_exp (List.hd (main_deadlock_solver [result] true)) in
      (det_res_loop (use_subst::tl))
  else
    hd::(det_res_loop tl)

let rec final_change exp dexps solved =
  match dexps, solved with
  | [], [] -> exp
  | hd::tl, [] -> exp
  | hd::tl, hd1::tl1 -> 
    let deadl_exp = find_deadl_exp exp hd in
    let use_subst = use_lsubst deadl_exp hd1 in
      final_change use_subst tl tl1

let main exp =
  try
    Printexc.record_backtrace true;
    let lamExp = toLambda exp in
    (* Process Completeness Verification *)
    let act_ver = main_act_verifier lamExp in
    if has_miss_acts act_ver then
      (printMode fmt lamExp true; printf "\n"; print_act_ver act_ver)
    else 
      (* Deadlock detection *)
      let toList = lparToList lamExp  in
      let res = 
        if List.length toList <= 2 then
          (eval [[(lamExp, {print=true; level="1"})]])
        else
          let comb_lst = topComb toList in 
            printFinalArrComb fmt (flatten2 comb_lst);
            eval (assign_ctx2 comb_lst true)
      in
      let flat_res = flatten2 res in
      let init_findings = (proc_findings_comb flat_res) in
      if List.length init_findings = 0 then
        printf "\nThe process is deadlock-free.\n"
      else
        if List.length init_findings = List.length flat_res then (
          printf "\nThe process has a deadlock: every process combination is blocked.\n";
          print_list_comb fmt (rev flat_res);
          (*let all_solv = det_res_loop [(List.hd (List.filter (fun x -> x <> LNil) (fst (flatten2 res))))] in*)
          let all_solv = det_res_loop (List.filter (fun x -> x <> LNil) (fst (flatten2 res))) in
          let final_res = final_change lamExp (List.filter (fun x -> x <> LNil) (map lchi_to_lpar (fst flat_res))) all_solv in
          printf "\nDeadlock(s) solved with algorithm %d:\n" !ds;
          printMode fmt final_res true
        ) else (
          printf "\nThe process has a deadlock: some process combination is blocked.\n";
          print_list_comb fmt (rev (flatten2 res));
          let all_solv = det_res_loop (List.filter (fun x -> x <> LNil) (fst (flatten2 res))) in
          let final_res = final_change lamExp (List.filter (fun x -> x <> LNil) (map lchi_to_lpar (fst (flatten2 res)))) all_solv in
          printf "\nDeadlock(s) solved with algorithm %d:\n" !ds;
          if final_res = lamExp then 
            let filter_res = List.filter ( fun (a,_) -> a <> LNil ) (flatten2 res) in
            let alter_res = main_deadlock_solver (fst filter_res) false in
            let rem_nils = map remLNils alter_res in
            let f_res = List.combine rem_nils (snd filter_res) in
            print_list_comb fmt f_res
          else
            printMode fmt final_res true
        )
  with
  | _ -> Printexc.print_backtrace stdout
;;


(* -- Deadlock -- *)
(* 1) (a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0    --->    Case with complete (global) resolution *)
main (CCS.parse "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0");

(* 2) a! || (b!.b?.a? + a?)    --->    Case with partial (local) resolution *)
(* main (parse "a! || (b!.b?.a? + a?)"); *)


(* -- Actions missing correspondence    --->    No resolution *)
(* 3) a?.(c?.0 + d?.0) || a!.e!.0 *)
(* main ( PPar(PPref(AIn('a'), POr(PPref(AIn('c'), PNil), PPref(AIn('d'), PNil))), PPref(AOut('a'), PPref(AOut('e'), PNil))) )  *)
(* main (parse "a! || (b!.b?.a? + a?)"); *)


(* -- No deadlock -- *)
(* 4) a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0 *)
(* main (parse "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0"); *)