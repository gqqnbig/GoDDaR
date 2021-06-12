open Format
open Types
open Printer

(* ------------------- EXCEPTIONS -------------------- *)

exception RuntimeException of string

(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

let usage_msg = "Usage: ./dlock [-v | -s] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let simplified = ref false
let procInput = ref false

let speclist =
    [("-v", Arg.Set verbose, "Output debug information");
     ("-s", Arg.Set simplified, "Output a simpler representation of the process");
     ("-p", Arg.Set procInput, "Input is a Proc type, which is translated to a Lamba type");
     ("  ", Arg.Unit (fun () -> ()), "Output the verdict only")]

let () = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg


(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Finds the index of elem in list *)
let rec find list elem i =
    match list with
    | [] -> -1 (* Not found *)
    | hd::tl -> if hd = elem then i else find tl elem (i+1)

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

(* Removes the EEta from a LChi's LList at index i_at *)
let rec correct_chi_lambda ll curr_i i_at =
    match ll with
    | [] -> raise (RuntimeException "correct_chi_lambda failed: Empty lambda list")
    | hd::tl -> if curr_i = i_at then 
        (match hd with
        | LList(EEta(_), LList(EEta(a), l)) -> LList(EEta(a), l)::tl
        | LList(EEta(_), LNil) -> LNil::tl
        | _ -> raise (RuntimeException "correct_chi_lambda failed: No match"))
        else hd::(correct_chi_lambda tl (curr_i+1) i_at)

(* Similar to correct_chi_lambda, but returns EEta at index i_at instead of removing it *)
let rec find_chi_lambda chi curr_i i_at =
    match chi with
    | LChi(et, hd::tl) -> if curr_i = i_at then
        (match hd with
        | LList(a, LList(b, l)) -> a
        | LList(a, LNil) -> a
        | _ -> raise (RuntimeException "find_chi_lambda failed: Unexpected match")) (* Pode ser preciso para o 3º caso*)
        else find_chi_lambda (LChi(et, tl)) (curr_i+1) i_at

(* Pulls the next Eta and arranges Chi's ll*)
let case_e elem chi =
    match chi with
    | LChi(el, ll) -> 
        let at_index = find el elem 0 in
            let nth_elem = List.nth el at_index in
                LChi(subst_first el nth_elem (find_chi_lambda chi 0 at_index), correct_chi_lambda ll 0 at_index)

(* Finds the indexes of the first pair of corresponding actions *)
let rec find_corres list dlist i j = 
    match list, dlist with
    | [], [] -> (-1, -1) (* Not found *)
    | [], dhd::dtl -> find_corres dtl dtl (j+1) (j+1)
    | hd::tl, dhd::dtl ->
        (match hd, dhd with
        | EEta(AIn(a)), EEta(AOut(b)) when a = b -> if i < j then (i,j) else (j,i)
        | EEta(AOut(a)), EEta(AIn(b)) when a = b -> if i < j then (i,j) else (j,i)
        | _, _ -> find_corres tl dlist (i+1) j)

(* Assuming two Etas in a Chi's el are equal, this function pulls the two next Etas and arranges the Chi's ll *)
(* Example case: chi = (a? | b? | c! | b!; a!, d?, z?, d!) *)
let case_f chi =
    match chi with
    | LChi(el, ll) ->
        let at_indexes = find_corres el el 0 0 in
        match at_indexes with
        | (a, b) -> 
            let nth_elemA = List.nth el a in 
                let nth_elemB = List.nth el b in
                    let chi_lambdaA = find_chi_lambda chi 0 a in
                        let chi_lambdaB = find_chi_lambda chi 0 b in
                            LChi(subst_first (subst_first el nth_elemA chi_lambdaA) nth_elemB chi_lambdaB , correct_chi_lambda (correct_chi_lambda ll 0 a) 0 b)

(* Checks whether there are corresponding actions in list *)
let rec exist_corres list =
    match list with
    | [] -> false
    | hd::tl -> 
        match hd with
        | EEta(AIn(a)) -> List.exists ((=) (EEta(AOut(a)))) tl || exist_corres tl
        | EEta(AOut(a)) -> List.exists ((=) (EEta(AIn(a)))) tl || exist_corres tl

(* Defines the joining of two LChi or between a LList and a LChi *)
let join_chis lhs rhs = 
    match lhs, rhs with
    | LChi(le, ll), LChi(re, rl) -> LChi(le@re, ll@rl)
    | LChi(le, ll), LList(e, lst) -> LChi(le@[e], ll@[lst])
    | LList(e, lst), LChi(le, ll) -> LChi(le@[e], ll@[lst])
    | _, _ -> raise(RuntimeException "join_chi failed: Not a chi or list.")

(* Checks if elem exists in el by trying to get its index. If so, also checks if there is a LChi at i_at in ll. *)
(* Returns true if both conditions are met, otherwise returns false. *)
let exists_and_chi elem chi =
    match chi with
    | LChi(el, ll) -> 
        let i_at = find el elem 0 in
            if i_at = -1 then false else
                let res = List.filteri(
                    fun i a -> 
                        if i = i_at then 
                            match a with
                            | LChi(_, _) -> true
                            | _ -> false
                        else false) ll in 
                            if List.length res = 1 then true else false

(* Retrieves the LChi at position i_at in list *)
let get_chi_at list i_at =
    let f_list = List.filteri (fun i _ -> if i = i_at then true else false) list in (List.hd f_list)

(* Defines the case when there are two correspondent actions and a Chi must be pulled from the level below *)
(* Example case: a?0 x (b! | a!; 0, (c! | d?; 0, 0)) -> 0 x (b! | c! | d?; 0; 0; 0) *)
let case_g elem chi =
    match chi with
    | LChi(el, ll) ->
        let i_at = find el elem 0 in
            let f_el = List.filteri ( fun i _ -> if i!=i_at then true else false) el in
                let f_ll = List.filteri ( fun i _ -> if i!=i_at then true else false) ll in
                    join_chis (LChi(f_el, f_ll)) (get_chi_at ll i_at)

(* Function that contains all the reduction cases *)
(* Falta fazer todos os casos com lhs e rhs trocados *)
let eval_par lhs rhs =
    match lhs, rhs with
    | LList(EEta(a),l1), LList(EEta(b),l2) ->
        begin
            match a, b with
            | AIn(k), AOut(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B*)
            | AOut(k), AIn(j) -> if k = j then LPar(l1, l2) else LChi([EEta(a);EEta(b)], l1::l2::[]) (* Case A and Case B *)
            | _, _ -> LChi([EEta(a);EEta(b)], l1::l2::[])                                            (* Case B *)
        end
    | LList(EEta(a), l1), LChi(EEta(b)::EEta(c)::l2, l3) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)                                      (* Case C *)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))                                     (* Case D *)
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lhs, lc)                                      (* Case C *)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lhs, lb))                                     (* Case D *)
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) rhs -> LPar(l1, case_g (EEta(AIn(k))) rhs)    (* Case G *)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) rhs -> LPar(l1, case_g (EEta(AOut(k))) rhs)   (* Case G *)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(LList(EEta(a), l1), case_f (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case F *)
            | AOut(k), _ , _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                   then LPar(l1, case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
                                   else join_chis lhs rhs
            | AIn(k), _ , _, _  -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                   then LPar(l1, case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3))) (* Case E *)
                                   else join_chis lhs rhs
        end
    | LChi(EEta(b)::EEta(c)::l2, l3), LList(EEta(a), l1) ->
        begin
            match a, b, c, l3 with
            | _, AIn(k), AOut(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lc, rhs)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lb, rhs))
            | _, AOut(k), AIn(j), [lb; lc] when k = j && List.length l2 = 0 ->
                (match lb, lc with
                | LNil, LChi(hd::tl, hd1::tl1) -> LPar(lc, rhs)
                | LChi(hd::tl, hd1::tl1), LNil -> LPar(lb, rhs))
            | AOut(k), _, _, _ when exists_and_chi (EEta(AIn(k))) lhs -> LPar(case_g (EEta(AIn(k))) lhs, l1)
            | AIn(k), _, _, _ when exists_and_chi (EEta(AOut(k))) lhs -> LPar(case_g (EEta(AOut(k))) lhs, l1)
            | _, _, _, _ when exist_corres (EEta(b)::EEta(c)::l2) -> LPar(case_f (LChi(EEta(b)::EEta(c)::l2, l3)), LList(EEta(a), l1))
            | AOut(k), _, _, _ -> if List.exists ((=) (EEta(AIn(k)))) (EEta(b)::EEta(c)::l2) 
                                  then LPar(case_e (EEta(AIn(k))) (LChi(EEta(b)::EEta(c)::l2, l3)), l1)
                                  else join_chis lhs rhs
            | AIn(k), _, _, _ -> if List.exists ((=) (EEta(AOut(k)))) (EEta(b)::EEta(c)::l2)
                                 then LPar(case_e (EEta(AOut(k))) (LChi(EEta(b)::EEta(c)::l2, l3)), l1)
                                 else join_chis lhs rhs
        end
    | LNil, _ -> rhs
    | _, LNil -> lhs
    | _, _ -> LPar(lhs, rhs) (* Results in infinite loop *)

let rec eval exp =
    if !verbose then
        let _ = print_lambdas fmt exp in let _ = printf " ---> " in let _ = print_proc_simple fmt (toProc exp) in printf "\n"
    else if !simplified then
        let _ = print_proc_simple fmt (toProc exp) in printf "\n"
    else ();
    match exp with
    | LNil -> LNil
    | LList(e, l) -> exp
    | LPar(LNil, LPar(l3, l4)) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4) (* These cases exist to remove the prints *)
    | LPar(LPar(l3,l4), LNil) -> eval (let e3 = eval l3 in let e4 = eval l4 in eval_par e3 e4)  (* of redudant LNil evaluations *)
    | LPar(l1, l2) -> eval (let e1 = eval l1 in let e2 = eval l2 in eval_par e1 e2)
    | LChi(el, ll) -> exp

let main exp =
    let res = eval exp in
    match res with
    | LNil -> printf "The process is deadlock-free.\n"
    | _ -> printf "The process has a deadlock.\n"
;;

(* ------------------- TESTING -------------------- *)
main (LPar(LList(EEta(AOut('a')), LPar(LList(EEta(AOut('b')), LNil) , LList(EEta(AIn('b')), LNil))), LList(EEta(AIn('a')), LNil)));;


