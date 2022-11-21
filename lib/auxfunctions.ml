(* Definition of auxiliary functions *)
open Types
open Printer
open List2

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
  | LNil -> true
  | LPar(l1, l2) -> hasLNil l1 || hasLNil l2
  | _ -> false

(** Strips the expression from containing [LNil] located inside [LPar] *)
let rec remLNils exp: 'a lambda_base =
  let rec rem exp =
    match exp with
    | LPar(LNil, LNil) -> LNil
    | LPar(a, LNil) -> rem a
    | LPar(LNil, b) -> rem b 
    | LPar(a, b) -> LPar(rem a, rem b)
    | _ -> exp
  in
  let currExp = ref exp in
  while hasLNil !currExp && !currExp != LNil do
      currExp := rem !currExp
  done; 
  !currExp
