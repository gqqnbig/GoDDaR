open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector2


(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 

;;


if (!process <> "") then
  ignore (main (CCS.parse !process))