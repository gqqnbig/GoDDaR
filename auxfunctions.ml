(* Definition of auxiliary functions *)

open Types

(* ------------------- AUXILIARY FUNCTIONS -------------------- *)

(* Enumerates the elements of a list by transforming them into pairs *)
let rec enumerate list i =
    match list with
    | [] -> []
    | hd::tl -> (i, hd)::(enumerate tl (i+1))

(* Returns the index of the permutations of the inital process that lead to a deadlock *)
let proc_findings lst = 
    List.filter (fun y -> if y = -1 then false else true) 
        (List.map (fun x -> 
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
    | hd::tl, _ -> (head::hd::(toAdd@tl))::(combinations head tl (toAdd@[hd]))

(* Iterates the list and applies the combinations function to every element, returning the combinations for the entire list *)
(* Given comb ['a';'b';'c'] [], outputs [[['a'; 'b'; 'c']; ['a'; 'c'; 'b']]; [['b'; 'c'; 'a']]; []] (which is then flattened) *)
let rec comb list toAdd = 
    match list, toAdd with
    | [], _ -> []
    | hd::tl, [] -> (combinations hd tl [])::(comb tl (hd::toAdd))
    | hd::tl, _ -> (combinations hd tl toAdd)::(comb tl (toAdd@[hd]))

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
        | [x;t] -> (pairExprs [hd])@(loopl tl)
        | [x;m;t] -> ((loopl (pairExprs (combinations x [m;t] [])))@loopl tl)
        | x::t -> ((loopl (List.map (fun z -> x::z) (pairExprs (List.flatten (comb t []))))@(loopl tl))))    

(* Top-level function for the combination of functions *)
let rec topComb list =
    let comb_res = List.flatten (comb list []) in
        let prdExprs = pairExprs comb_res in
            loopl prdExprs

let proc_findings_comb lst =
    List.filter (fun y -> if y = None then false else true)
    (List.map ( fun x -> 
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

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg