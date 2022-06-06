open Types

(* ------------------- BEGIN CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let rec has_lor exp =
  match exp with
  | LList(a, b) -> has_lor b
  | LPar(a, b) -> (has_lor a)||(has_lor b)
  | LOrI(_,_) -> true
  | _ -> false

let starts_w_lpar exp =
  match exp with
  | LPar(_,_) -> true
  | _ -> false

let starts_w_lor exp =
  match exp with
  | LOrI(_, _) -> true
  | _ -> false

let starts_w_llist exp =
  match exp with
  | LList(_,_) -> true
  | _ -> false

let has_inner_lor exp =
  match exp with
  | LList(a, LOrI(_,_)) -> true
  | LList(a, b) -> false

let rec has_lor exp =
  match exp with
  | LPar(a, b) -> (has_lor a)||(has_lor b)
  | LList(a, b) -> has_lor b
  | LOrI(_,_) -> true
  | LOrE(_,_) -> true
  | LNil -> false

let rec lor_disentangler exp =
  match exp with
  | LOrI(a, b) -> (lor_disentangler a)@(lor_disentangler b)
  | _ -> [exp]

(** [exp] must start with LList

    Returns list of all possible executions wrt. LOrI *)
let rec inner_lor_dis exp =

  (** Returns [exp] with only the LLists until an LOrI *)
  let rec calc_prev_exp exp = 
    match exp with
    | LList(a, LOrI(_,_)) -> LList(a, LNil)
    | LList(a, b) -> LList(a, calc_prev_exp b)
  in
  let prev_exp = calc_prev_exp exp in
  (** Returns the first LOrI after a sequence of LList *)
  let rec calc_lor_exp exp =
    match exp with
    | LList(a, (LOrI(_,_) as x)) -> x
    | LList(a, b) -> calc_lor_exp b 
  in 
  let lor_exp = calc_lor_exp exp in
  (** Receives the first LOrI of the [exp],
      Returns a list of lambda, of the left side of succesive LOrI
      *)
  let rec lors exp =
    match exp with
    | LList(a, LNil) as e -> [e]
    | LList(a, b) as e -> [e]
    | LPar(a, b) as e -> [e]
    | LOrI(x, y) -> y::(lors x)
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
  (** List of lambda, [lors_arr] with each element prefixed with [prev_exp]*)
  let one_pass_res = arr_loop lors_arr in
  let rec any_has_lor arr =
    match arr with
    | [] -> []
    | hd::tl ->
      if has_lor hd then
        (List.rev (inner_lor_dis hd))@(any_has_lor tl)
      else
        hd::(any_has_lor tl)
  in any_has_lor one_pass_res
let rec lpar_lor_case exp =
  let rec calc_lor_exp exp =
    match exp with
    | LPar(LOrI(a, b), LOrI(c, d)) -> LPar(a, c)::LPar(a, d)::LPar(b, c)::LPar(b, d)::[]
    | LPar(LOrI(a, b), x        ) -> LPar(a, x)::LPar(b, x)::[]
    | LPar(x        , LOrI(a, b)) -> LPar(x, a)::LPar(x, b)::[]

    | LPar(LPar(LOrI(a, b), LOrI(c, d)), x) -> LPar(LPar(a, c), x)::LPar(LPar(a, d), x)::LPar(LPar(b, c), x)::LPar(LPar(b, d), x)::[]
    | LPar(LPar(LOrI(a, b), c        ), x) -> LPar(LPar(a, c), x)::LPar(LPar(b,c), x)::[]
    | LPar(LPar(a        , LOrI(b, c)), x) -> LPar(LPar(a, b), x)::LPar(LPar(a, c), x)::[]

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

    | LPar((LList(_,_) as y), (_ as z)         ) when has_lor y -> let inner_res = inner_lor_dis y in List.map (fun x -> LPar(x, z)) inner_res
    | LPar((_ as y)         , (LList(_,_) as z)) when has_lor z -> let inner_res = inner_lor_dis z in List.map (fun x -> LPar(y, x)) inner_res
    (* Maches above??? *)
    | LPar((LPar(_, _) as y), (LList(_,_) as z)) when has_lor y -> let res = calc_lor_exp y in List.map (fun x -> LPar(x, z)) res
    | LPar((LList(_,_) as y), (LPar(_,_) as z) ) when has_lor z -> let res = calc_lor_exp z in List.map (fun x -> LPar(y, x)) res 
    | _ -> exp::[]
  in
  let one_pass_res = calc_lor_exp exp in
  let rec any_has_lor arr =
    match arr with
    | [] -> []
    | hd::tl ->
      if has_lor hd then
        (lpar_lor_case hd)@(any_has_lor tl)
      else
        hd::(any_has_lor tl)
  in
  any_has_lor one_pass_res

let rec lor_assign arr =
  match arr with
  | [] -> []
  | hd::tl ->
    if has_lor hd then
      (inner_lor_dis hd)::(lor_assign tl)
    else
      [hd]::(lor_assign tl)

let rec reduce_arr arr i =
  match arr with
  | (a, b)::[] -> (a, i+b)
  | (a, b)::tl -> reduce_arr tl (i+b)

(** Returns a list of pairs (acting as a map/associative array) with the first item being the 
    [eta] action and the second being the counter of occurences of prefixes with that action

    *)
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
    | (a, b)::tl -> 
      let f_arr = List.filter ( fun (c, _) -> c = a) arr in
      let red_res = reduce_arr f_arr 0 in
      let rem_arr = List.filter ( fun (c, _) -> c <> a) arr in
        red_res::(reduce rem_arr)
  in reduce count_res

(** Receives [(eta * int) list], list of pairs (eta, int), with int being the count of that [eta]
    actions in the expression.

    Returns the list of [eta]s with an unequal counter compared to [compl_eta eta] *)
let rec compare_action_counts (arr : (eta * int) list) = 
  (** Like List.remove_assoc, but removes all, not just the first *)
  let rec rem_assocs eta arr = 
    if List.mem_assoc eta arr then
      rem_assocs eta (List.remove_assoc eta arr)
    else
      arr
  in
  match arr with
  | [] -> []
  | (eta, eta_count)::tl ->
    try
      let eta' = compl_eta eta in
      let i = List.assoc eta' arr in
      let filter_compl = List.filter (fun (c, i) -> eta' <> c) tl in
      if eta_count = i then
        compare_action_counts (rem_assocs eta' (rem_assocs eta arr)) (* Usar o filter_compl aqui retira apenas os complementos todos, e não as restantes ações *)
      else (
        if eta_count > i then
          eta::(compare_action_counts filter_compl)
        else
          eta'::(compare_action_counts filter_compl)
      )
    with
    | Not_found -> eta::(compare_action_counts tl)

(** Checks if [b' list] is not empty *)
let rec has_miss_acts arr =
  match arr with
  | [] -> false
  | (a,b)::tl -> ((List.length b) > 0) || has_miss_acts tl

(** Receives a lambda representation of a process

    Returns [(lambda * eta list) list]. Generates a list of lambdas, (all the possible executions?)
    and for each one return a list of [eta]/actions with an unequal occurence counter compared to
    [compl_eta eta] *)
let main_act_verifier exp =
  (** Receives a list of lambda.

      Returns [(lambda * eta list) list], with each pair being, the expression from the first list
      with a list of the [eta]s with an unequal occurence counter compared to [compl_eta eta] *)
  let rec count_loop arr =
    match arr with
    | [] -> []
    | hd::tl -> (
      let count_res = count_actions hd in
      let comp_res = compare_action_counts count_res in
      (hd, comp_res)::(count_loop tl))
  in
  if starts_w_lor exp then
    let disent = lor_disentangler exp in
    let lpar_lor_res = 
      List.map (fun x ->
        if not (has_lor x) then
          [x]
        else
          if starts_w_llist x then
            inner_lor_dis x
          else
            lpar_lor_case x
      ) disent in
    count_loop (List.flatten lpar_lor_res)
  else
    let lpar_lor_res = lpar_lor_case exp in
    count_loop lpar_lor_res

(* ------------------- END CORRESPONDING ACTIONS VERIFICATION ------------------- *)