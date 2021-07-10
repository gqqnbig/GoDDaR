(* Definition of types and some conversion functions *)

(* ---------- Types ----------  *)

type chan = char

type action =
    | AIn of chan
    | AOut of chan

type proc = 
    | PNil
    | POr of proc * proc
    | PPref of action * proc
    | PPar of proc * proc

type eta = 
    | EEta of action

type lambda = 
    | LNil
    | LOr of lambda * lambda
    | LList of eta * lambda
    | LChi of eta list * lambda list
    | LPar of lambda * lambda

(* Print_context *)
type print_ctx =
{
    print   :   bool;
    level   :   string;
}

(* ---------- Functions ----------  *)

let rec toLambda proc =
    match proc with
    | PNil -> LNil
    | POr(a, b) -> LOr(toLambda a, toLambda b)
    | PPref(a, p) -> LList(EEta(a), toLambda p)
    | PPar(p1, p2) -> LPar(toLambda p1, toLambda p2)

let toAction eta =
    match eta with
    | EEta(a) -> a

let rec toProc lambda =
    match lambda with
    | LNil -> PNil
    | LOr(a, b) -> POr(toProc a, toProc b)
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

let assign_ctx lst =
    let i = ref 0 in
        List.map (fun x -> i:=!i+1; (x, {print = true; level = string_of_int !i})) lst

let assign_ctx2 lst =
    let i = ref 0 in
        List.map ( fun x -> 
            List.map ( fun y -> i:=!i+1; (y, {print = true; level = string_of_int !i})) x
        ) lst

let next_ctx ctx = {ctx with level = ctx.level ^ ".1"}

let conc_lvl ctx lvl = {ctx with level = ctx.level ^ "." ^ lvl}

let compl_eta eta =
    match eta with
    | EEta(AIn(k)) -> EEta(AOut(k))
    | EEta(AOut(k)) -> EEta(AIn(k))

let isLPar exp =
    match exp with
    | LPar(_,_) -> true
    | _ -> false

let isLChi exp =
    match exp with
    | LChi(_, _) -> true
    | _ -> false

let isLOr exp =
    match exp with
    | LOr(_,_) -> true
    | _ -> false

let getEl chi =
    match chi with
    | LChi(el, ll) -> el

let getL1 lpar =
    match lpar with
    | LPar(l1, l2) -> l1

let getL2 lpar =
    match lpar with
    | LPar(l1, l2) -> l2
