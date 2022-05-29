open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector


(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 

;;


if (!process <> "") then
  ignore (main Format.std_formatter (CCS.parse !process))