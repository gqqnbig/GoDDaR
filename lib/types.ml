open Cmd
open Format

let null_fmt = Format.make_formatter ( fun _ _ _ -> () ) (fun _ -> ())

(* ---------- Types ----------  *)

type chan = string

type action =
    | AIn of chan
    | AOut of chan

let print_action fmt a =
    match a with
    | AIn(a) -> fprintf fmt "AIn(%s)" a
    | AOut(a) -> fprintf fmt "AOut(%s)" a

let print_action_simple fmt a =
    match a with
    | AIn(a) -> fprintf fmt "%s?" a
    | AOut(a) -> fprintf fmt "%s!" a


module type Eta_type =
  sig 
    type eta
    val print_eta: formatter -> eta -> unit
    val print_eta_simple: formatter -> eta -> unit
    val print_etalist: formatter -> eta list -> unit
  end


module Eta =
  struct
  type eta =
    | EEta of action

  let print_eta fmt e = 
      match e with
      | EEta(a) -> fprintf fmt "EEta(%a)" print_action a
  let print_eta_simple fmt e = 
      match e with
      | EEta(a) -> print_action_simple fmt a

  let rec print_etalist_alt fmt lst =
    match lst with
    | [] -> ()
    | hd::[] -> print_eta fmt hd
    | hd::tl -> print_eta fmt hd; fprintf fmt ", "; print_etalist_alt fmt tl

  let rec print_etalist_alt_simple fmt lst =
    match lst with
    | [] -> ()
    | EEta(k)::[] ->
      print_action_simple fmt k
    | EEta(k)::tl ->
      print_action_simple fmt k; fprintf fmt ", "; print_etalist_alt_simple fmt tl

  let rec print_etalist fmt lst =
      match lst with
      | [] -> ()
      | hd::[] -> fprintf fmt "%a" print_eta hd
      | hd::tl -> fprintf fmt "%a | " print_eta hd; print_etalist fmt tl

  end


module EtaTagged =
  struct
  type eta =
    | EEta of action * string
  let print_eta fmt (e: eta) = 
      match e with
      | EEta(a, i) -> fprintf fmt "EEtaTagged(%a, %s)" print_action a i
  let print_eta_simple fmt e = 
      match e with
      | EEta(a, _) -> print_action_simple fmt a
  let rec print_etalist fmt lst =
      match lst with
      | [] -> ()
      | hd::[] -> fprintf fmt "%a" print_eta hd
      | hd::tl -> fprintf fmt "%a | " print_eta hd; print_etalist fmt tl
  end


module Lambda_Base(Eta_base: Eta_type) =
    struct
    type t = 
        | LNil
        | LOrI of t * t
        | LOrE of t * t
        | LList of Eta_base.eta * t
        | LPar of t * t
        | LRepl of Eta_base.eta * t

    let isLPar (exp: t) =
        match exp with
        | LPar(_,_) -> true
        | _ -> false
    
    let isLOrI exp =
        match exp with
        | LOrI(_,_) -> true
        | _ -> false

  (* Retrieves the lambdas from a LPar type and adds them to a list *)
  let rec lparToList exp = 
    match exp with
    | LPar(l, r) -> lparToList l @ lparToList r
    | _ -> [exp]

  let rec assocLeftList list =
    match list with
    | [] -> LNil
    | [hd] -> hd
    | hd::tl::[] -> LPar(tl, hd)
    | hd::md::tl -> LPar(LPar(assocLeftList tl, md), hd)
  
  let rec assocLOrIList list = 
    match list with
    | [] -> LNil
    | [hd] -> hd
    | hd::tl::[] -> LOrI(tl, hd)
    | hd::md::tl -> LOrI(hd, LOrI(md, assocLOrIList tl))
  
  let rec assocLOrEList list = 
    match list with
    | [] -> LNil
    | [hd] -> hd
    | hd::tl::[] -> LOrE(hd, tl)
    | hd::md::tl -> LOrE(hd, LOrE(md, assocLOrEList tl))
  
  let rec hasLNil exp =
    match exp with
    | LNil -> false
    | LPar(a, b) -> (a = LNil) || (b = LNil)
    | LOrE(a, b) | LOrI(a, b) ->
      hasLNil a || hasLNil b
    | LList(_, l) | LRepl(_, l) -> hasLNil l

  (** Strips the expression from containing [LNil] located inside [LPar] *)
  let rec remLNils exp =
    let rec rem exp =
      match exp with
      | LPar(LNil, LNil) -> LNil
      | LPar(a, LNil) -> rem a
      | LPar(LNil, b) -> rem b 
      | LPar(a, b) -> LPar(rem a, rem b)
      | LOrE(a, b) -> LOrE(rem a, rem b)
      | LOrI(a, b) -> LOrI(rem a, rem b)
      | LList(e, l) -> LList(e, rem l)
      | LRepl(e, l) -> LRepl(e, rem l)
      | LNil -> LNil
    in
    let currExp = ref exp in
    while hasLNil !currExp && !currExp <> LNil do
        currExp := rem !currExp
    done; 
    !currExp


  let rec print_lambda fmt l =
      match l with
      | LNil -> fprintf fmt "LNil"
      | LOrI(l1, l2) -> fprintf fmt "LOrI(%a, %a)" print_lambda l1 print_lambda l2
      | LOrE(l1, l2) -> fprintf fmt "LOrE(%a, %a)" print_lambda l1 print_lambda l2
      | LList(e1, l1) -> fprintf fmt "LList(%a, %a)" Eta_base.print_eta e1 print_lambda l1
      | LPar(l1, l2) -> fprintf fmt "LPar(%a, %a)" print_lambda l1 print_lambda l2
      | LRepl(e1, l1) -> fprintf fmt "LRepl(%a, %a)" Eta_base.print_eta e1 print_lambda l1
  and print_lambdalist fmt lst =
      match lst with
      | [] -> ()
      | hd::[] -> fprintf fmt "%a" print_lambda hd
      | hd::tl -> fprintf fmt "%a, " print_lambda hd; print_lambdalist fmt tl

  let rec print_lambda_simple fmt l =
      match l with
      | LNil -> fprintf fmt "0"
      | LOrI(p1, p2) -> fprintf fmt "(%a + %a)" print_lambda_simple p1 print_lambda_simple p2
      | LOrE(p1, p2) -> fprintf fmt "(%a & %a)" print_lambda_simple p1 print_lambda_simple p2
      | LList(e, pp) -> fprintf fmt "%a.%a" Eta_base.print_eta_simple e print_lambda_simple pp
      | LPar(p1, p2) -> fprintf fmt "(%a || %a)" print_lambda_simple p1 print_lambda_simple p2
      | LRepl(e, pp) -> fprintf fmt "*%a.%a" Eta_base.print_eta_simple e print_lambda_simple pp

  let print fmt exp =
    if !verbose then (
      fprintf fmt "%a ---> %a" print_lambda_simple exp print_lambda exp;
    ) else (
      print_lambda_simple fmt exp;
    );
  end

module Lambda = 
  struct 
  include Lambda_Base(Eta)

  end

module LambdaTagged =
  struct
    include Lambda_Base(EtaTagged)
    
  end

type ctx = { level   :   string; }

let printCtxLevel fmt ctx =
    fprintf fmt "---- %s ----" ctx.level

(* ---------- Functions ----------  *)

let rec lambdaToLambdaTagged (exp: Lambda.t): LambdaTagged.t = 
    let i = ref 0 in
    let rec etaToEtaTagged (EEta(a): Eta.eta): EtaTagged.eta =
        EEta(a, (i := !i+1; string_of_int !i))
    in
    let rec do_lambdaToLambdaTagged (exp: Lambda.t): LambdaTagged.t =
        match exp with
        | LNil -> LNil
        | LPar(a, b) -> LPar(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LOrI(a, b) -> LOrI(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LOrE(a, b) -> LOrE(do_lambdaToLambdaTagged a, do_lambdaToLambdaTagged b)
        | LList(e, l) -> LList(etaToEtaTagged e, do_lambdaToLambdaTagged l)
        | LRepl(e, l) -> LRepl(etaToEtaTagged e, do_lambdaToLambdaTagged l)
    in do_lambdaToLambdaTagged exp

let rec etaTaggedToEta (EEta(a, _): EtaTagged.eta): Eta.eta =
    EEta(a)

let rec lambdaTaggedToLambda (exp: LambdaTagged.t): Lambda.t = 
    match exp with
    | LNil -> LNil
    | LPar(a, b) -> LPar(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LOrI(a, b) -> LOrI(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LOrE(a, b) -> LOrE(lambdaTaggedToLambda a, lambdaTaggedToLambda b)
    | LList(e, l) -> LList(etaTaggedToEta e, lambdaTaggedToLambda l)
    | LRepl(e, l) -> LRepl(etaTaggedToEta e, lambdaTaggedToLambda l)

let toAction (eta: Eta.eta) =
    match eta with
    | EEta(a) -> a

let actionToString action: string = 
  match action with
  | AIn(c) -> c
  | AOut(c) -> c

let assign_ctx lst print =
    let i = ref 0 in
        List.map (fun x -> i:=!i+1; (x, {level = string_of_int !i})) lst

let assign_ctx2 lst print =
    let i = ref 0 in
        List.map ( fun x -> 
            List.map ( fun y -> i:=!i+1; (y, {level = string_of_int !i})) x
        ) lst

let next_ctx ctx = {level = ctx.level ^ ".1"}

let conc_lvl ctx lvl = {level = ctx.level ^ "." ^ lvl}

let compl_action action = 
    match action with
    | AIn(k) -> AOut(k)
    | AOut(k) -> AIn(k)

let compl_eta (eta: Eta.eta): Eta.eta =
    match eta with
    | EEta(action) -> EEta(compl_action action)


(** This is an attempt to provide a canonical version of the lambda type, such that it can be compared
and sorted correctly *)
module LambdaC_base(Eta_base: Eta_type) =
    struct
    type t = 
      | LList of Eta_base.eta * t
      | LPar of t list
      | LOrI of t list
      | LOrE of t list
      | LRepl of Eta_base.eta * t
      | LNil
    
    let compare = compare

    let rec lambdaLParToLambdaCLPar (exp: Lambda_Base(Eta_base).t): t = 
      let rec do_lambdaLParToLambdaCLPar (exp: Lambda_Base(Eta_base).t): t list = 
        match exp with
        | LPar(l, r) -> (do_lambdaLParToLambdaCLPar l) @ (do_lambdaLParToLambdaCLPar r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LPar(List.sort compare (do_lambdaLParToLambdaCLPar exp))
    and lambdaLOrIToLambdaCLOrI (exp: Lambda_Base(Eta_base).t): t = 
      let rec do_lambdaLOrIToLambdaCLOrI (exp: Lambda_Base(Eta_base).t): t list = 
        match exp with
        | LOrI(l, r) -> (do_lambdaLOrIToLambdaCLOrI l) @ (do_lambdaLOrIToLambdaCLOrI r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LOrI(List.sort compare (do_lambdaLOrIToLambdaCLOrI exp))
    and lambdaLOrEToLambdaCLOrE (exp: Lambda_Base(Eta_base).t): t = 
      let rec do_lambdaLOrEToLambdaCLOrE (exp: Lambda_Base(Eta_base).t): t list = 
        match exp with
        | LOrE(l, r) -> (do_lambdaLOrEToLambdaCLOrE l) @ (do_lambdaLOrEToLambdaCLOrE r)
        | LNil -> []
        | _ -> [lambdaToLambdaC exp]
      in
      LOrE(List.sort compare (do_lambdaLOrEToLambdaCLOrE exp))
    and lambdaToLambdaC (lambda: Lambda_Base(Eta_base).t): t = 
      match lambda with
      | LList(eta, l) -> LList(eta, lambdaToLambdaC l)
      | LPar(_, _) -> lambdaLParToLambdaCLPar lambda
      | LOrI(_, _) -> lambdaLOrIToLambdaCLOrI lambda
      | LOrE(_, _) -> lambdaLOrEToLambdaCLOrE lambda
      | LRepl(eta, l) -> LRepl(eta, lambdaToLambdaC l)
      | LNil -> LNil
    
    let rec lambdaCToLambda (lambda: t): Lambda_Base(Eta_base).t = 
      let rec fold_LPar list: Lambda_Base(Eta_base).t =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LPar(tl, hd)
        | hd::md::tl -> LPar(LPar(fold_LPar tl, md), hd)
      in
      let rec fold_LOrI list: Lambda_Base(Eta_base).t =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LOrI(tl, hd)
        | hd::md::tl -> LOrI(LOrI(fold_LOrI tl, md), hd)
      in
      let rec fold_LOrE list: Lambda_Base(Eta_base).t =
        match list with
        | [] -> LNil
        | [hd] -> hd
        | hd::tl::[] -> LOrE(tl, hd)
        | hd::md::tl -> LOrE(LOrE(fold_LOrE tl, md), hd)
      in
      match lambda with
      | LList(eta, l) -> LList(eta, lambdaCToLambda l)
      | LPar(lambda_list) -> fold_LPar (List.map lambdaCToLambda lambda_list)
      | LOrI(lambda_list) -> fold_LOrI (List.map lambdaCToLambda lambda_list)
      | LOrE(lambda_list) -> fold_LOrE (List.map lambdaCToLambda lambda_list)
      | LRepl(eta, l) -> LRepl(eta, lambdaCToLambda l)
      | LNil -> LNil

    let canonicalizeLambda lambda =
        lambdaCToLambda (lambdaToLambdaC lambda)
end

module LambdaC = LambdaC_base(Eta)
module LambdaCTagged = LambdaC_base(EtaTagged)


let rec eta_equals_eta_tagged (exp1: LambdaC.t) (exp2: LambdaCTagged.t): bool =
match exp1, exp2 with
| LList(EEta(c1), l), LList(EEta(c2, _), r) when c1 = c2 -> eta_equals_eta_tagged l r
| LRepl(EEta(c1), l), LRepl(EEta(c2, _), r) when c1 = c2 -> eta_equals_eta_tagged l r
| LPar(l), LPar(r)
| LOrI(l), LOrI(r)
| LOrE(l), LOrE(r) -> List.for_all2 (fun l r -> eta_equals_eta_tagged l r) l r
| LNil, LNil -> true
| _, _ -> false