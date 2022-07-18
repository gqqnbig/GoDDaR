open Dlock
open Dlock.Types
;;

List.map (fun c -> (LList(EEta(AIn(c)), LNil))) ["a"; "b"; "c"; "d"; "e"; "f"]
|> Auxfunctions.topComb 
|> List.flatten
|> List.iter (fun l -> Printer.print_proc_simple Format.std_formatter (toProc l); Format.printf "\n");

(*Ok, topComp is bugged
  There should be (6!/2=360) even permutation of 6 elements, it generates 540, some of which are repeated.......  *)