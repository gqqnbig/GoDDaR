(* Definition of auxiliary functions *)
open Dlock.Types
open Printer

(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Tail recursive map *)
let map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l

(* Tail recursive rev *)
let rev l =
    let rec rev l r = match l with
        | [] -> r
        | (h::t) -> rev t (h::r)
    in
    rev l []

(* Tail recursive array append *)
let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Tail recursive flatten *)
let flatten2 ll =
    let rec go acc = function
    | [] -> rev acc
    | l :: r -> go (List.rev_append l acc) r
in
    go [] ll

(* Enumerates the elements of a list by transforming them into pairs *)
let rec enumerate list i =
    match list with
    | [] -> []
    | hd::tl -> (i, hd)::(enumerate tl (i+1))

(* Returns the index of the permutations of the inital process that lead to a deadlock *)
let proc_findings lst = 
    List.filter (fun y -> if y = -1 then false else true) 
        (map (fun x -> 
            match x with 
            | (a,b) -> if b = LNil then -1 else a) (enumerate lst 0))

(* Finds the index of elem in list *)
let rec find list elem =
    match list with
    | [] -> -1 (* Not found *)
    | hd::tl -> if hd = elem then 0 else 1 + find tl elem 

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

let rec subst_at list replacer curr_i i_at =
    match list with
    | [] -> []
    | hd::tl -> if curr_i = i_at then replacer::tl else hd::(subst_at tl replacer (curr_i+1) i_at)

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
    List.filter (fun y -> if y = None then false else true)
    (map ( fun x -> 
        match x with
        | (LNil, _) -> None
        | (_, ctx) -> Some ctx.level
    ) lst)

let rec find_all_corres list dlist i j = 
    match list, dlist with
    | [], [] -> [] 
    | [], dhd::dtl -> find_all_corres dtl dtl (j+1) (j+1)
    | hd::tl, dhd::dtl ->
        (match hd, dhd with
        | EEta(AIn(a)), EEta(AOut(b)) when a = b -> if i < j then (i,j)::(find_all_corres tl dlist (i+1) j) else (j,i)::(find_all_corres tl dlist (i+1) j)
        | EEta(AOut(a)), EEta(AIn(b)) when a = b -> if i < j then (i,j)::(find_all_corres tl dlist (i+1) j) else (j,i)::(find_all_corres tl dlist (i+1) j)
        | _, _ -> find_all_corres tl dlist (i+1) j)

let rec find_corres_list el eta i =
    match el with
    | [] -> []
    | hd::tl -> if hd = eta then i::(find_corres_list tl eta (i+1)) else find_corres_list tl eta (i+1)

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
    | LPar(LNil, _) | LPar(LList(_, _), _) | LPar(LChi(_,_), _) | LPar(LOr(_,_), _) -> exp
    | LPar(l1, _) -> getNestedLeft l1

let rec getParNum exp =
    match exp with
    | LPar(LNil, _) | LPar(LList(_, _), _) | LPar(LChi(_,_), _) | LPar(LOr(_,_), _) -> 2
    | LPar(l1, _) -> 1 + getParNum l1
    | _ -> 0

let rec getRestPars list =
    let i = ref 0 in List.filter (fun x -> i:= !i+1; if !i < 3 then false else true) list

let addAfterChiEval2 pair exp =
    match pair with
    | (a, ctx) -> (LPar(a, exp), ctx)

let addAfterChiEval list exp =
    List.map (
        fun x -> 
            match x with
            | (a, ctx) -> ((LPar(a,exp)), ctx)
    ) list

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
    | LChi(el, ll) | LPar(LChi(el, ll), LNil) | LPar(LNil, LChi(el, ll)) 
    | LPar(LChi(el, ll), LList(_,_)) | LPar(LList(_,_), LChi(el, ll)) -> if find_all_corres el el 0 0 != [] then true else false
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

let rec has_lor_at ll pair =
    match pair with
    | (a, b) ->
        let a_ll = List.nth ll a in
        let b_ll = List.nth ll b in
        match a_ll, b_ll with
        | LOr(_,_), LOr(_,_) | LOr(_,_), _ | _, LOr(_,_) -> true
        | _, _ -> false

(* ------------------- BEGIN CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let rec has_lor exp =
  match exp with
  | LList(a, b) -> has_lor b
  | LPar(a, b) -> (has_lor a)||(has_lor b)
  | LOr(_,_) -> true
  | _ -> false

let starts_w_lpar exp =
  match exp with
  | LPar(_,_) -> true
  | _ -> false

let starts_w_lor exp =
  match exp with
  | LOr(_, _) -> true
  | _ -> false

let starts_w_llist exp =
  match exp with
  | LList(_,_) -> true
  | _ -> false

let has_inner_lor exp =
  match exp with
  | LList(a, LOr(_,_)) -> true
  | LList(a, b) -> false

let rec has_inner_lor_2 exp =
  match exp with
  | LPar(a, b) -> (has_inner_lor_2 a)||(has_inner_lor_2 b)
  | LList(a, b) -> has_inner_lor_2 b
  | LOr(_,_) -> true
  | LNil -> false

let rec lor_disentangler exp =
  match exp with
  | LOr(a, b) -> (lor_disentangler a)@(lor_disentangler b)
  | _ -> [exp]

let rec inner_lor_dis exp =
  let rec calc_prev_exp exp = 
    match exp with
    | LList(a, LOr(_,_)) -> LList(a, LNil)
    | LList(a, b) -> LList(a, calc_prev_exp b)
  in
  let prev_exp = calc_prev_exp exp in
  let rec calc_lor_exp exp =
    match exp with
    | LList(a, (LOr(_,_) as x)) -> x
    | LList(a, b) -> calc_lor_exp b 
  in 
  let lor_exp = calc_lor_exp exp in
  let rec lors exp =
    match exp with
    | LList(a, LNil) as e -> [e]
    | LList(a, b) as e -> [e]
    | LPar(a, b) as e -> [e]
    | LOr(x, y) -> y::(lors x)
  in 
  let lors_arr = lors lor_exp in
  let rec join exp subst =
    match exp with
    | LList(a, LNil) -> LList(a, subst)
    | LList(a, b) -> LList(a, join b subst)
  in
  let rec arr_loop arr =
    match arr with
    | [] -> []
    | hd::tl -> (join prev_exp hd)::(arr_loop tl)
  in
  let one_pass_res = arr_loop lors_arr in
  let rec any_has_lor arr =
    match arr with
    | [] -> []
    | hd::tl -> if has_lor hd then (List.rev (inner_lor_dis hd))@(any_has_lor tl) else hd::(any_has_lor tl)
  in any_has_lor one_pass_res
and lpar_lor_case exp =
  let rec calc_lor_exp exp =
    match exp with
    | LPar(LOr(a, b), LOr(c, d)) -> LPar(a, c)::LPar(a, d)::LPar(b, c)::LPar(b, d)::[]
    | LPar(LOr(a, b), x) -> LPar(a, x)::LPar(b, x)::[]
    | LPar(x, LOr(a, b)) -> LPar(x, a)::LPar(x, b)::[]
    | LPar(LPar(LOr(a, b), LOr(c, d)), x) -> LPar(LPar(a, c), x)::LPar(LPar(a, d), x)::LPar(LPar(b, c), x)::LPar(LPar(b, d), x)::[]
    | LPar(LPar(LOr(a, b), c), x) -> LPar(LPar(a, c), x)::LPar(LPar(b,c), x)::[]
    | LPar(LPar(a, LOr(b, c)), x) -> LPar(LPar(a, b), x)::LPar(LPar(a, c), x)::[]
    | LPar((LList(_,_) as y), (LList(_,_) as z)) when has_inner_lor y && has_inner_lor z ->
      let inner_res_y = inner_lor_dis y in
      let inner_res_z = inner_lor_dis z in
      let rec assoc_loop y_arr z_arr z_c =
        match y_arr, z_arr with
        | [], _ -> []
        | hd::tl, [] -> assoc_loop tl z_c z_c
        | hd::tl, h::t -> LPar(hd, h)::(assoc_loop y_arr t z_c)
      in 
      assoc_loop inner_res_y inner_res_z inner_res_z
    | LPar((LList(_,_) as y), (_ as z)) when has_lor y -> let inner_res = inner_lor_dis y in List.map (fun x -> LPar(x, z)) inner_res
    | LPar((_ as y), (LList(_,_) as z)) when has_lor z -> let inner_res = inner_lor_dis z in List.map (fun x -> LPar(y, x)) inner_res
    | LPar((LPar(_, _) as y), (LList(_,_) as z)) when has_lor y -> let res = calc_lor_exp y in List.map (fun x -> LPar(x, z)) res
    | LPar((LList(_,_) as y), (LPar(_,_) as z)) when has_lor z -> let res = calc_lor_exp z in List.map (fun x -> LPar(y, x)) res 
    | _ -> exp::[]
  in
  let one_pass_res = calc_lor_exp exp in
  let rec any_has_lor arr =
    match arr with
    | [] -> []
    | hd::tl -> if has_lor hd then (lpar_lor_case hd)@(any_has_lor tl) else hd::(any_has_lor tl)
  in
  any_has_lor one_pass_res
     
let rec lor_assign arr =
  match arr with
  | [] -> []
  | hd::tl -> if has_lor hd then (inner_lor_dis hd)::(lor_assign tl) else [hd]::(lor_assign tl)

let rec reduce_arr arr i =
  match arr with
  | (a, b)::[] -> (a, i+b)
  | (a, b)::tl -> reduce_arr tl (i+b)

let rec count_actions exp =
  let rec count exp arr =
    match exp with
    | LNil -> arr
    | LPar(a, b) -> (count a arr)@(count b []) (* Caso contrário ficamos com ocorrências duplicadas depois da concatenaçaõ *)
    | LList(a, b) ->
        try 
          let i = List.assoc a arr in
          let r_arr = List.remove_assoc a arr in
          let n_arr = (a, i+1)::r_arr in
          count b n_arr
        with
        | Not_found -> count b ((a, 1)::arr)
  in
  let count_res = count exp [] in
  let rec reduce arr =
    match arr with
    | [] -> []
    | ((a, b) as hd)::tl -> 
      let f_arr = 
        List.filter (
          fun x ->
          match x with
          | (c, i) -> if c = a then true else false
        ) arr in
      let red_res = reduce_arr f_arr 0 in
      let rem_arr = 
        List.filter (
          fun x ->
          match x with
          | (c, i) -> if c != a then true else false
        ) arr in
        red_res::(reduce rem_arr)
  in reduce count_res

let rec compare_action_counts arr = 
  let rec rem_assocs eta arr = 
    if List.mem_assoc eta arr
    then rem_assocs eta (List.remove_assoc eta arr)
    else arr
  in
  match arr with
  | [] -> []
  | (a, b)::tl ->
    try
      let i = List.assoc (compl_eta a) arr in
      let filter_compl a =
        List.filter (
          fun x ->
          match x with
          | (c, i) -> if (compl_eta a) = c then false else true
        ) tl in
      if b = i
      then compare_action_counts (rem_assocs (compl_eta a) (rem_assocs a arr)) (* Usar o filter_compl aqui retira apenas os complementos todos, e não as restantes ações *)
      else (
        if b > i then
          a::(compare_action_counts (filter_compl a))
        else (compl_eta a)::(compare_action_counts (filter_compl a))
      )
    with
    | Not_found -> a::(compare_action_counts tl)

let rec has_miss_acts arr =
  match arr with
  | [] -> false
  | (a,b)::tl -> ((List.length b) > 0) || has_miss_acts tl

let main_act_verifier exp =
  let rec count_loop arr =
    match arr with
    | [] -> []
    | hd::tl -> (
      let count_res = count_actions hd in
      let comp_res = compare_action_counts count_res in
      (hd, comp_res)::(count_loop tl))
  in
  if starts_w_lor exp
  then
    let disent = lor_disentangler exp in
    let lpar_lor_res = 
      List.map (fun x ->
        if has_inner_lor_2 x = false
        then [x]
        else
          if starts_w_llist x
          then inner_lor_dis x
          else lpar_lor_case x
      ) disent in
    count_loop (List.flatten lpar_lor_res)
  else
    let lpar_lor_res = lpar_lor_case exp in
    count_loop lpar_lor_res


  
(* ------------------- END CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let rec rem_print_ctx arr =
  match arr with
  | [] -> []
  | hd::tl ->
    match hd with
    | (a, b) -> a::(rem_print_ctx tl)

let rec all_same arr =
  match arr with
  | [hd] -> true
  | [hd; tl] -> if hd = tl then true else false
  | hd::md::tl -> if hd = md then all_same (md::tl) else false

let fst arr = map (fun x -> fst x) arr

let snd arr = map (fun x -> snd x) arr

let rec use_lsubst exp sub =
  match exp with
  | LSubst -> sub
  | LPar(a, b) -> LPar(use_lsubst a sub, use_lsubst b sub)
  | LOr(a, b) -> LOr(use_lsubst a sub, use_lsubst b sub)
  | LList(a, b) -> LList(a, use_lsubst b sub)
  | LNil -> LNil



