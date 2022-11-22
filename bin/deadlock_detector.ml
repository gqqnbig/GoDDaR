open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector
open Types


(* ------------------- COMMAND LINE -------------------- *)

let process = ref ""
let migo = ref ""

let speclist =
    [("-v", Arg.Set verbose, "Output debug information");
     ("-ds", Arg.Int (fun i -> if (i!=1 && i!= 2) then raise (Arg.Bad ("Bad argument: Select deadlock resolution algorithm (1 or 2)")) else ds:=i), "Select deadlock resolution algorithm (1 or 2)");
     ("-p", Arg.Set_string process, "Process this process");
     ("-m", Arg.Set_string migo, "Convert and process MiGo file")
    ]

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg

;;


let ccs =
  if (!process <> "") then
    CCS.parse (!process)
  else if (!migo <> "") then (
    (MiGo_to_CCS.migo_to_ccs (MiGo.parse_file !migo))
  ) else (
    Format.printf "%s\n" Cmd.usage_msg;
    raise (Arg.Bad ("Give a CCS process or a MiGo file"))
  )
  in
ignore (main Format.std_formatter ccs)