open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector

(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 
;;

if (!process = "") then (

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

(* Crashes *)
(* main (CCS.parse "a!.a?.0 || b!.b?.0 || ( c!.c?.0 + (d!.d?.0 || e!.e?.0) )"); *)

(* main (CCS.parse "a!.b?.0 || b!.a?.0 || c!.d?.0 || d!.c?.0"); *)
(* main (CCS.parse "a!.b?.0 || a?.b!.0 || a!.a?.0"); *)

(* main (CCS.parse "a!.b?.0 || a?.b!.0 || (a!.a?.0 + b!.b?.0)"); *)
(* main (CCS.parse "a!.b?.0 || b!.a?.0"); *)
(* main (CCS.parse "( c?.0 + s?.0 ) || (c!.0 + s!.0)"); *)

main (CCS.parse "a!.a?.0" )
) else
  main (CCS.parse !process)