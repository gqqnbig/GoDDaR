(* Definition of auxiliary functions *)
open Types
open Types.Eta
open Printer
open List2

(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Returns the index of the permutations of the inital process that lead to a deadlock *)
let proc_findings lst = 
  List.filter (fun y -> y <> -1) 
    (map (fun (a, b) -> if b = LNil then -1 else a) (enumerate lst 0))

(* Outputs the possible combinations, with the head being fixed at the lhs *)
(* Given combinations 'a' ['b';'c'] [], outputs [['a';'b';'c']; ['a';'c';'b']] *)
let rec combinations head tail toAdd =
  match tail, toAdd with
  | [], _ -> []
  | hd::tl, [] -> (head::hd::tl)::(combinations head tl (hd::toAdd))
  | hd::tl, _ -> (head::hd::(append toAdd tl))::(combinations head tl (append toAdd [hd]))

(* Iterates the list and applies the combinations function to every element, returning the combinations for the entire list *)
(* Given comb ['a';'b';'c'] [], outputs [[['a'; 'b'; 'c']; ['a'; 'c'; 'b']]; [['b'; 'c'; 'a']]; []] (which is then flattened) *)
let rec comb list toAdd = 
  match list, toAdd with
  | [], _ -> []
  | hd::tl, [] -> (combinations hd tl [])::(comb tl (hd::toAdd))
  | hd::tl, _ -> (combinations hd tl toAdd)::(comb tl (append toAdd [hd]))

(* Given a list of combinations, this function pairs the first two elements every time it is called *)
let rec pairExprs exp =
  match exp with
  | [] -> []
  | hd::tl -> 
    (match hd with
    | [] -> []
    | h::m::t -> (((LPar(h,m))::t))::(pairExprs tl))

(* Loops the entire procedure of combining the elements and pairing *)
let rec loopl pExprs =
  match pExprs with
  | [] -> []
  | hd::tl -> 
    (match hd with
    | [x;t] -> append (pairExprs [hd]) (loopl tl)
    | [x;m;t] -> (append (loopl (pairExprs (combinations x [m;t] []))) (loopl tl))
    | x::t -> append (loopl (map (fun z -> x::z) (pairExprs (flatten2 (comb t []))))) (loopl tl))

(* Top-level function for the combination of functions *)
let rec topComb list =
  let comb_res = flatten2 (comb list []) in
  let prdExprs = pairExprs comb_res in
  loopl prdExprs

let proc_findings_comb lst =
  List.filter (fun y -> y <> None)
    (map ( function
      | (LNil, _) -> None
      | (_, ctx) -> Some ctx.level
    ) lst)

let rec find_all_corres list dlist i j = 
  match list, dlist with
  | [], [] -> [] 
  | [], dhd::dtl -> find_all_corres dtl dtl (j+1) (j+1)
  | hd::tl, dhd::dtl ->
    (match hd, dhd with
    | EEta(AIn(a)), EEta(AOut(b)) when a = b ->
      if i < j then
        (i,j)::(find_all_corres tl dlist (i+1) j)
      else
        (j,i)::(find_all_corres tl dlist (i+1) j)
    | EEta(AOut(a)), EEta(AIn(b)) when a = b ->
      if i < j then
        (i,j)::(find_all_corres tl dlist (i+1) j)
      else
        (j,i)::(find_all_corres tl dlist (i+1) j)
    | _, _ -> find_all_corres tl dlist (i+1) j)

let rec find_corres_list el eta i =
  match el with
  | [] -> []
  | hd::tl ->
    if hd = eta then
      i::(find_corres_list tl eta (i+1))
    else
      find_corres_list tl eta (i+1)

(* Retrieves the lambdas from a LPar type and adds them to a list *)
let rec lparToList exp = 
  match exp with
  | LPar(l, r) -> lparToList l @ lparToList r
  | _ -> [exp]

let assocLeft exp =
  let toList = lparToList exp in
  let rec assoc list =
    match list with
    | [hd] -> hd
    | hd::tl::[] -> LPar(tl, hd)
    | hd::md::tl -> LPar(LPar(assoc tl, md), hd)
  in assoc (List.rev toList)

let assocLeftList list =
  let rec assoc list =
    match list with
    | [hd] -> hd
    | hd::tl::[] -> LPar(hd, tl)
    | hd::md::tl -> LPar(LPar(hd, md), assoc tl)
  in assoc list

let rec getNestedLeft exp =
  match exp with
  | LPar(LNil, _)
  | LPar(LList(_, _), _)
  | LPar(LChi(_,_), _)
  | LPar(LOr(_,_), _) -> exp
  | LPar(l1, _) -> getNestedLeft l1

let rec getParNum exp =
  match exp with
  | LPar(LNil, _)
  | LPar(LList(_, _), _)
  | LPar(LChi(_,_), _)
  | LPar(LOr(_,_), _) -> 2
  | LPar(l1, _) -> 1 + getParNum l1
  | _ -> 0

let rec getRestPars list =
  let i = ref 0 in
  List.filter (fun x -> i:= !i+1; !i >= 3) list

let addAfterChiEval2 (a, ctx) exp =
  (LPar(a, exp), ctx)

let addAfterChiEval list exp =
  List.map ( fun (a, ctx) -> ((LPar(a,exp)), ctx) ) list

let rec hasLNil exp =
  match exp with
  | LNil -> true
  | LPar(l1, l2) -> hasLNil l1 || hasLNil l2
  | _ -> false

let rec remLNils exp =
  let rec rem exp =
    match exp with
    | LPar(LNil, LNil) -> LNil
    | LPar(_ as a, LNil) -> rem a
    | LPar(LNil, (_ as b)) -> rem b 
    | LPar(a, b) -> LPar(rem a, rem b)
    | _ -> exp
  in
  let currExp = ref exp in
  while hasLNil !currExp && !currExp != LNil do
      currExp := rem !currExp
  done; 
  !currExp

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
  | LPar(LList(EEta(AIn(j)), l), LChi(el, ll)) -> find_corres_list el (EEta(AOut(j))) 0 != [] || find_all_corres el el 0 0 != []
  | LPar(LChi(el,ll), LList(EEta(AOut(j)), l)) 
  | LPar(LList(EEta(AOut(j)), l), LChi(el, ll)) -> find_corres_list el (EEta(AIn(j))) 0 != [] || find_all_corres el el 0 0 != []
  | LChi(el, ll) | LPar(LChi(el, ll), _) | LPar(_, LChi(el, ll)) -> find_all_corres el el 0 0 != []
  | LPar(l1, _) -> can_chi_progress l1
  | _ -> false

let rec can_chi_list_progress exp =
  match exp with
  | LPar(LChi(el, ll), LList(EEta(AIn(j)), l))
  | LPar(LList(EEta(AIn(j)), l), LChi(el, ll)) -> find_corres_list el (EEta(AOut(j))) 0 != []
  | LPar(LChi(el,ll), LList(EEta(AOut(j)), l)) 
  | LPar(LList(EEta(AOut(j)), l), LChi(el, ll)) -> find_corres_list el (EEta(AIn(j))) 0 != []
  | LPar(l1, _) -> can_chi_list_progress l1
  | _ -> false

let rec can_chi_nested_progress exp =
  match exp with
  | LChi(el, ll) | LPar(LChi(el, ll), LNil) | LPar(LNil, LChi(el, ll)) 
  | LPar(LChi(el, ll), LList(_,_)) | LPar(LList(_,_), LChi(el, ll)) -> find_all_corres el el 0 0 != []
  | LPar(l1, _) -> can_chi_nested_progress l1
  | _ -> false

let rec has_nested_or exp =
  match exp with
  | LOr(_,_)
  | LPar(LOr(_,_), LNil) | LPar(LNil, LOr(_,_))
  | LPar(LOr(_,_), LList(_,_)) | LPar(LList(_,_), LOr(_,_))
  | LPar(LOr(_,_), LChi(_,_)) | LPar(LChi(_,_), LOr(_,_)) -> true
  | LPar(l1, _) -> has_nested_or l1
  | _ -> false

let rec has_lor_at ll (a, b) =
  let a_ll = List.nth ll a in
  let b_ll = List.nth ll b in
  match a_ll, b_ll with
  | LOr(_,_), LOr(_,_) | LOr(_,_), _ | _, LOr(_,_) -> true
  | _, _ -> false

let rec rem_print_ctx arr =
  match arr with
  | [] -> []
  | (a, b)::tl ->
    a::(rem_print_ctx tl)

let rec all_same arr =
  match arr with
  | [hd] -> true
  | [hd; tl] -> hd = tl
  | hd::md::tl ->
    if hd = md then
      all_same (md::tl)
    else
      false

let fst arr = map (fun x -> fst x) arr

let snd arr = map (fun x -> snd x) arr

let rec use_lsubst exp sub =
  match exp with
  | LSubst -> sub
  | LPar(a, b) -> LPar(use_lsubst a sub, use_lsubst b sub)
  | LOr(a, b) -> LOr(use_lsubst a sub, use_lsubst b sub)
  | LList(a, b) -> LList(a, use_lsubst b sub)
  | LNil -> LNil


