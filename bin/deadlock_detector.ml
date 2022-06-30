open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector


(* ------------------- COMMAND LINE -------------------- *)

let () = cmdParse 

;;


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