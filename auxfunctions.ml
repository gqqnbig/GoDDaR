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