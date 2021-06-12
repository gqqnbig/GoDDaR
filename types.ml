(* Definition of types and some conversion functions *)

(* ---------- Types ----------  *)

type chan = char

type action =
    | AIn of chan
    | AOut of chan

type proc = 
    | PNil
    | PPref of action * proc
    | PPar of proc * proc

type eta = 
    | EEta of action

type lambda = 
    | LNil
    | LList of eta * lambda
    | LChi of eta list * lambda list
    | LPar of lambda * lambda

(* ---------- Functions ----------  *)

let rec toLambda proc =
    match proc with
    | PNil -> LNil
    | PPref(a, p) -> LList(EEta(a), toLambda p)
    | PPar(p1, p2) -> LPar(toLambda p1, toLambda p2)

let toAction eta =
    match eta with
    | EEta(a) -> a

let rec toProc lambda =
    match lambda with
    | LNil -> PNil
    | LList(e, l) -> PPref(toAction e, toProc l)
    | LPar(l1, l2) -> PPar(toProc l1, toProc l2)
    | LChi(et, ll) -> chi_to_proc lambda
and chi_to_proc chi =
    match chi with
    | LChi(ehd::[], lhd::[]) -> PPref(toAction ehd, toProc lhd)
    | LChi(ehd::etl, lhd::ltl) -> 
        let c_ehd = toAction ehd in
        let c_lhd = toProc lhd in PPar(PPref(c_ehd, c_lhd), chi_to_proc (LChi(etl, ltl)))