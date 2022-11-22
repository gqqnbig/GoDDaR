(* Definition of printing functions for Action, Lambda and Proc types*)
open Format
open Types
open Cmd

let null_fmt = Format.make_formatter ( fun _ _ _ -> () ) (fun _ -> ())



(* ----------- Misc ----------- *)


let count_level print_ctx =
  let count = ref 0 in
    String.iter (fun c -> if c = '.' then count := !count + 1) print_ctx.level;
    !count

let printCtxLevel_noln fmt p_ctx =
  if p_ctx.print then
    let level = count_level p_ctx in
    if !verbose || !simplified then
      fprintf fmt "---- %s ----\n" p_ctx.level
  else ()

(* ------------------- BEGIN CORRESPONDING ACTIONS VERIFICATION ------------------- *)

let print_act_ver fmt (arr: (Lambda.t * Eta.eta list) list) =
  let rec print arr =
    match arr with
    | [] -> ()
    | hd::tl -> 
        match hd with
        | (a, []) -> print tl
        | (a, b) -> 
          if !verbose then (
            fprintf fmt "- ";
            Eta.print_etalist_alt fmt b;
            fprintf fmt " in ";
            Lambda.printMode fmt a true;
            fprintf fmt "\n";
            print tl
          ) else if !simplified then (
            (fprintf fmt "- ";
            Eta.print_etalist_alt_simple fmt b;
            fprintf fmt " in ";
            Lambda.printMode fmt a true;
            print tl)
          )
  in
  if !verbose || !simplified then
    fprintf fmt "Action(s) missing correspondence(s) in process(es):\n";
    print arr