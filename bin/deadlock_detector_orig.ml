open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector_orig

(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 
;;

(* -- Deadlock -- *)
(* 1) (a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0    --->    Case with complete (global) resolution *)
(* main (CCS.parse "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0"); *)

(* 2) a! || (b!.b?.a? + a?)    --->    Case with partial (local) resolution *)
(* main (parse "a! || (b!.b?.a? + a?)"); *)


(* -- Actions missing correspondence    --->    No resolution *)
(* 3) a?.(c?.0 + d?.0) || a!.e!.0 *)
(* main ( PPar(PPref(AIn('a'), POrI(PPref(AIn('c'), PNil), PPref(AIn('d'), PNil))), PPref(AOut('a'), PPref(AOut('e'), PNil))) )  *)
(* main (parse "a! || (b!.b?.a? + a?)"); *)


(* -- No deadlock -- *)
(* 4) a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0 *)
(* main (parse "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0"); *)

let ccs = CCS.parse (
  if (!process <> "") then
    !process
  else if (!migo <> "") then (
    (MiGo_to_CCS.migo_to_ccs (MiGo.parse_file !migo))
  ) else 
    failwith "Give a CCS process or a MiGo file plz"
)
  in
ignore (main Format.std_formatter ccs)