open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector

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
(* main ( PPar(PPref(AIn('a'), POr(PPref(AIn('c'), PNil), PPref(AIn('d'), PNil))), PPref(AOut('a'), PPref(AOut('e'), PNil))) )  *)
(* main (parse "a! || (b!.b?.a? + a?)"); *)


(* -- No deadlock -- *)
(* 4) a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0 *)
(* main (parse "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0"); *)

if (!process <> "") then
  ignore (main (CCS.parse !process))