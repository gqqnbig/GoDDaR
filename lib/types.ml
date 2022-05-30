(* Definition of types and some conversion functions *)

(* ---------- Types ----------  *)

type chan = char

type action =
    | AIn of chan
    | AOut of chan

type proc = 
    | PNil
    | POrI of proc * proc
    | POrE of proc * proc
    | PPref of action * proc
    | PPar of proc * proc

type eta = 
    | EEta of action

type eta_tagged = 
    | EEtaTagged of action * int

type 'a lambda_base = 
    | LNil
    | LOrI of 'a lambda_base * 'a lambda_base
    | LOrE of 'a lambda_base * 'a lambda_base
    | LList of 'a * 'a lambda_base
    | LChi of 'a list * 'a lambda_base list
    | LPar of 'a lambda_base * 'a lambda_base
    | LSubst

type lambda = eta lambda_base

type lambda_tagged = eta_tagged lambda_base

(* Print_context *)
type print_ctx =
{
    print   :   bool;
    level   :   string;
}

(* ---------- Functions ----------  *)

let rec lambdaToLambdaTagged (exp: lambda): lambda_tagged = 
    let i = ref 0 in
    let rec etaToEtaTagged (EEta(a): eta): eta_tagged =
        EEtaTagged(a, (i := !i+1; !i))
    in
    let rec do_lambdaToLambdaTagged exp =
        match exp with
        | LNil -> LNil
        | LSubst -> LSubst
        | LPar(a, b) -> LPar(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LChi(a, b) -> LChi(List.map etaToEtaTagged a, List.map (do_lambdaToLambdaTagged) b)
        | LOrI(a, b) -> LOrI(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LOrE(a, b) -> LOrE(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LList(e, l) -> LList(etaToEtaTagged e, do_lambdaToLambdaTagged l)
    in do_lambdaToLambdaTagged exp

let rec etaTaggedToEta (EEtaTagged(a, _): eta_tagged): eta =
    EEta(a)

let rec lambdaTaggedToLambda (exp: lambda_tagged): lambda = 
    match exp with
    | LNil -> LNil
    | LSubst -> LSubst
    | LPar(a, b) -> LPar(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LChi(a, b) -> LChi(List.map etaTaggedToEta a, List.map (lambdaTaggedToLambda) b)
    | LOrI(a, b) -> LOrI(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LOrE(a, b) -> LOrE(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LList(e, l) -> LList(etaTaggedToEta e, lambdaTaggedToLambda l)

let rec toLambda proc =
    match proc with
    | PNil -> LNil
    | POrI(a, b) -> LOrI(toLambda a, toLambda b)
    | POrE(a, b) -> LOrE(toLambda a, toLambda b)
    | PPref(a, p) -> LList(EEta(a), toLambda p)
    | PPar(p1, p2) -> LPar(toLambda p1, toLambda p2)

let toAction eta =
    match eta with
    | EEta(a) -> a

let rec toProc lambda =
    match lambda with
    | LNil -> PNil
    | LOrI(a, b) -> POrI(toProc a, toProc b)
    | LOrE(a, b) -> POrE(toProc a, toProc b)
    | LList(e, l) -> PPref(toAction e, toProc l)
    | LPar(l1, l2) -> PPar(toProc l1, toProc l2)
    | LChi(et, ll) -> chi_to_proc lambda
and chi_to_proc chi =
    match chi with
    | LChi([], []) -> PNil
    | LChi(ehd::[], lhd::[]) -> PPref(toAction ehd, toProc lhd)
    | LChi(ehd::etl, lhd::ltl) -> 
        let c_ehd = toAction ehd in
        let c_lhd = toProc lhd in PPar(PPref(c_ehd, c_lhd), chi_to_proc (LChi(etl, ltl)))

let assign_ctx lst print =
    let i = ref 0 in
        List.map (fun x -> i:=!i+1; (x, {print = print; level = string_of_int !i})) lst

let assign_ctx2 lst print =
    let i = ref 0 in
        List.map ( fun x -> 
            List.map ( fun y -> i:=!i+1; (y, {print = print; level = string_of_int !i})) x
        ) lst

let next_ctx ctx = {ctx with level = ctx.level ^ ".1"}

let conc_lvl ctx lvl = {ctx with level = ctx.level ^ "." ^ lvl}

let compl_action action = 
    match action with
    | AIn(k) -> AOut(k)
    | AOut(k) -> AIn(k)

let compl_eta eta =
    match eta with
    | EEta(action) -> EEta(compl_action action)

let isLPar exp =
    match exp with
    | LPar(_,_) -> true
    | _ -> false

let isLChi exp =
    match exp with
    | LChi(_, _) -> true
    | _ -> false

let isLOrI exp =
    match exp with
    | LOrI(_,_) -> true
    | _ -> false

let getEl chi =
    match chi with
    | LChi(el, ll) -> el

let getL1 lpar =
    match lpar with
    | LPar(l1, l2) -> l1
    | LOrI(l1, l2) -> l1

let getL2 lpar =
    match lpar with
    | LPar(l1, l2) -> l2
    | LOrI(l1, l2) -> l2

let lchi_to_lpar exp =
  match exp with
  | LChi(a, b) ->
    let rec loop arr1 arr2 =
      match arr1, arr2 with
      | hd::[], hd1::[] -> LList(hd, hd1)
      | hd::tl, hd1::tl1 -> LPar(LList(hd, hd1), loop tl tl1)
    in
    loop a b
  | _ -> exp

(** lambda flattned

This is an attempt to provide a canonical version of the lambda type, such that it can be compared
and sorted correctly *)
module LambdaFlattened =
    struct
    type 'a lambda_flattened = 
      | LList of 'a * 'a lambda_flattened
      | LPar of 'a lambda_flattened list
      | LOrI of 'a lambda_flattened list
      | LOrE of 'a lambda_flattened list
      | LNil
    
    type t = eta lambda_flattened

    let compare = compare

    let rec lambdaLParToLambdaFlattenedLPar (exp: lambda): 'a lambda_flattened = 
      let rec do_lambdaLParToLambdaFlattenedLPar (exp: lambda): 'a lambda_flattened list = 
        match exp with
        | LPar(l, r) -> (do_lambdaLParToLambdaFlattenedLPar l) @ (do_lambdaLParToLambdaFlattenedLPar r)
        | LNil -> []
        | _ -> [lambdaToLambdaFlattened exp]
      in
      LPar(List.sort compare (do_lambdaLParToLambdaFlattenedLPar exp))
    and lambdaLOrIToLambdaFlattenedLOrI (exp: 'a lambda_base): 'a lambda_flattened = 
      let rec do_lambdaLOrIToLambdaFlattenedLOrI (exp: lambda): 'a lambda_flattened list = 
        match exp with
        | LOrI(l, r) -> (do_lambdaLOrIToLambdaFlattenedLOrI l) @ (do_lambdaLOrIToLambdaFlattenedLOrI r)
        | LNil -> []
        | _ -> [lambdaToLambdaFlattened exp]
      in
      LOrI(List.sort compare (do_lambdaLOrIToLambdaFlattenedLOrI exp))
    and lambdaLOrEToLambdaFlattenedLOrI (exp: 'a lambda_base): 'a lambda_flattened = 
      let rec do_lambdaLOrEToLambdaFlattenedLOrI (exp: lambda): 'a lambda_flattened list = 
        match exp with
        | LOrE(l, r) -> (do_lambdaLOrEToLambdaFlattenedLOrI l) @ (do_lambdaLOrEToLambdaFlattenedLOrI r)
        | LNil -> []
        | _ -> [lambdaToLambdaFlattened exp]
      in
      LOrI(List.sort compare (do_lambdaLOrEToLambdaFlattenedLOrI exp))
    and lambdaToLambdaFlattened (lambda: 'a lambda_base): 'a lambda_flattened = 
      match lambda with
      | LList(eta, l) -> LList(eta, lambdaToLambdaFlattened l)
      | LPar(_, _) -> lambdaLParToLambdaFlattenedLPar lambda
      | LOrI(_, _) -> lambdaLOrIToLambdaFlattenedLOrI lambda
      | LOrE(_, _) -> lambdaLOrEToLambdaFlattenedLOrI lambda
      | LNil -> LNil
      | LChi(_,_) | LSubst -> failwith "not supported"
    
    let rec lambdaFlattenedToLambda (lambda_flattened: 'a lambda_flattened): 'a lambda_base = 
      let rec fold_LPar list: 'a lambda_base =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LPar(tl, hd)
        | hd::md::tl -> LPar(LPar(fold_LPar tl, md), hd)
      in
      let rec fold_LOrI list: 'a lambda_base =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LOrI(tl, hd)
        | hd::md::tl -> LOrI(LOrI(fold_LOrI tl, md), hd)
      in
      let rec fold_LOrE list: 'a lambda_base =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LOrE(tl, hd)
        | hd::md::tl -> LOrE(LOrE(fold_LOrI tl, md), hd)
      in
      match lambda_flattened with
      | LList(eta, l) -> LList(eta, lambdaFlattenedToLambda l)
      | LPar(lambda_list) -> fold_LPar (List.map lambdaFlattenedToLambda lambda_list)
      | LOrI(lambda_list) -> fold_LOrI (List.map lambdaFlattenedToLambda lambda_list)
      | LOrE(lambda_list) -> fold_LOrE (List.map lambdaFlattenedToLambda lambda_list)
      | LNil -> LNil

    let canonicalizeLambda lambda =
        lambdaFlattenedToLambda (lambdaToLambdaFlattened lambda)

end