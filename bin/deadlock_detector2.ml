open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector2


(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 

;;


if (!process = "") then (
  main (CCS.parse "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0")
) else
  main (CCS.parse !process)