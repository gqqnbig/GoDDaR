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
  ) else (
    Format.printf "%s\n" Cmd.usage_msg;
    raise (Arg.Bad ("Give a CCS process or a MiGo file"))
  )
)
  in
ignore (main Format.std_formatter ccs)