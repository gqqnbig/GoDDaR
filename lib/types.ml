(* Definition of types and some conversion functions *)

(* ---------- Types ----------  *)

type chan = string

type action =
    | AIn of chan
    | AOut of chan

type proc = 
    | PNil
    | POrI of proc * proc
    | POrE of proc * proc
    | PPref of action * proc
    | PPar of proc * proc
    | PRepl of action * proc

type eta = 
    | EEta of action

type eta_tagged = 
    | EEtaTagged of action * int

type 'a lambda_base = 
    | LNil
    | LOrI of 'a lambda_base * 'a lambda_base
    | LOrE of 'a lambda_base * 'a lambda_base
    | LList of 'a * 'a lambda_base
    | LPar of 'a lambda_base * 'a lambda_base
    | LRepl of 'a * 'a lambda_base

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
        | LPar(a, b) -> LPar(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LOrI(a, b) -> LOrI(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LOrE(a, b) -> LOrE(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LList(e, l) -> LList(etaToEtaTagged e, do_lambdaToLambdaTagged l)
        | LRepl(e, l) -> LRepl(etaToEtaTagged e, do_lambdaToLambdaTagged l)
    in do_lambdaToLambdaTagged exp

let rec etaTaggedToEta (EEtaTagged(a, _): eta_tagged): eta =
    EEta(a)

let rec lambdaTaggedToLambda (exp: lambda_tagged): lambda = 
    match exp with
    | LNil -> LNil
    | LPar(a, b) -> LPar(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LOrI(a, b) -> LOrI(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LOrE(a, b) -> LOrE(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LList(e, l) -> LList(etaTaggedToEta e, lambdaTaggedToLambda l)
    | LRepl(e, l) -> LRepl(etaTaggedToEta e, lambdaTaggedToLambda l)

let rec toLambda proc =
    match proc with
    | PNil -> LNil
    | POrI(a, b) -> LOrI(toLambda a, toLambda b)
    | POrE(a, b) -> LOrE(toLambda a, toLambda b)
    | PPref(a, p) -> LList(EEta(a), toLambda p)
    | PPar(p1, p2) -> LPar(toLambda p1, toLambda p2)
    | PRepl(a, p) -> LRepl(EEta(a), toLambda p)

let toAction eta =
    match eta with
    | EEta(a) -> a

let actionToString action: string = 
  match action with
  | AIn(c) -> c
  | AOut(c) -> c

let rec toProc lambda =
    match lambda with
    | LNil -> PNil
    | LOrI(a, b) -> POrI(toProc a, toProc b)
    | LOrE(a, b) -> POrE(toProc a, toProc b)
    | LList(e, l) -> PPref(toAction e, toProc l)
    | LPar(l1, l2) -> PPar(toProc l1, toProc l2)
    | LRepl(e, l) -> PRepl(toAction e, toProc l)

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

let isLOrI exp =
    match exp with
    | LOrI(_,_) -> true
    | _ -> false

(** This is an attempt to provide a canonical version of the lambda type, such that it can be compared
and sorted correctly *)
module LambdaC =
    struct
    type 'a lambdaC = 
      | LList of 'a * 'a lambdaC
      | LPar of 'a lambdaC list
      | LOrI of 'a lambdaC list
      | LOrE of 'a lambdaC list
      | LRepl of 'a * 'a lambdaC
      | LNil
    
    type t = eta lambdaC

    let compare = compare

    let rec eta_equals_eta_tagged (exp1: eta lambdaC) (exp2: eta_tagged lambdaC): bool =
    match exp1, exp2 with
    | LList(EEta(c1), l), LList(EEtaTagged(c2, _), r) when c1 = c2 -> eta_equals_eta_tagged l r
    | LRepl(EEta(c1), l), LRepl(EEtaTagged(c2, _), r) when c1 = c2 -> eta_equals_eta_tagged l r
    | LPar(l), LPar(r)
    | LOrI(l), LOrI(r)
    | LOrE(l), LOrE(r) -> List.for_all2 (fun l r -> eta_equals_eta_tagged l r) l r
    | LNil, LNil -> true
    | _, _ -> false

    let rec lambdaLParToLambdaCLPar (exp: 'a lambda_base): 'a lambdaC = 
      let rec do_lambdaLParToLambdaCLPar (exp: 'a lambda_base): 'a lambdaC list = 
        match exp with
        | LPar(l, r) -> (do_lambdaLParToLambdaCLPar l) @ (do_lambdaLParToLambdaCLPar r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LPar(List.sort compare (do_lambdaLParToLambdaCLPar exp))
    and lambdaLOrIToLambdaCLOrI (exp: 'a lambda_base): 'a lambdaC = 
      let rec do_lambdaLOrIToLambdaCLOrI (exp: 'a lambda_base): 'a lambdaC list = 
        match exp with
        | LOrI(l, r) -> (do_lambdaLOrIToLambdaCLOrI l) @ (do_lambdaLOrIToLambdaCLOrI r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LOrI(List.sort compare (do_lambdaLOrIToLambdaCLOrI exp))
    and lambdaLOrEToLambdaCLOrE (exp: 'a lambda_base): 'a lambdaC = 
      let rec do_lambdaLOrEToLambdaCLOrE (exp: 'a lambda_base): 'a lambdaC list = 
        match exp with
        | LOrE(l, r) -> (do_lambdaLOrEToLambdaCLOrE l) @ (do_lambdaLOrEToLambdaCLOrE r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LOrE(List.sort compare (do_lambdaLOrEToLambdaCLOrE exp))
    and lambdaToLambdaC (lambda: 'a lambda_base): 'a lambdaC = 
      match lambda with
      | LList(eta, l) -> LList(eta, lambdaToLambdaC l)
      | LPar(_, _) -> lambdaLParToLambdaCLPar lambda
      | LOrI(_, _) -> lambdaLOrIToLambdaCLOrI lambda
      | LOrE(_, _) -> lambdaLOrEToLambdaCLOrE lambda
      | LRepl(eta, l) -> LRepl(eta, lambdaToLambdaC l)
      | LNil -> LNil
    
    let rec lambdaCToLambda (lambdaC: 'a lambdaC): 'a lambda_base = 
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
        | hd::md::tl -> LOrE(LOrE(fold_LOrE tl, md), hd)
      in
      match lambdaC with
      | LList(eta, l) -> LList(eta, lambdaCToLambda l)
      | LPar(lambda_list) -> fold_LPar (List.map lambdaCToLambda lambda_list)
      | LOrI(lambda_list) -> fold_LOrI (List.map lambdaCToLambda lambda_list)
      | LOrE(lambda_list) -> fold_LOrE (List.map lambdaCToLambda lambda_list)
      | LRepl(eta, l) -> LRepl(eta, lambdaCToLambda l)
      | LNil -> LNil

    let canonicalizeLambda lambda =
        lambdaCToLambda (lambdaToLambdaC lambda)

end