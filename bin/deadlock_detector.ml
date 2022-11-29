open Format
open Dlock
open Dlock.Cmd
open Dlock.Deadlock_detector
open Types


(* ------------------- COMMAND LINE -------------------- *)

let process = ref ""
let migo = ref ""
let go_file = ref ""

let speclist =
    [("-v", Arg.Set verbose, "Output debug information");
     ("-ds", Arg.Int (fun i -> if (i!=1 && i!= 2) then raise (Arg.Bad ("Bad argument: Select deadlock resolution algorithm (1 or 2)")) else ds:=i), "Select deadlock resolution algorithm (1 or 2)");
     ("--go", Arg.Set go, "Output patched Go code");
     ("-p", Arg.Set_string process, "Process this process");
     ("-m", Arg.Set_string migo, "Convert and process MiGo file");
     ("-g", Arg.Set_string go_file, "Convert and process Go file");
    ]

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg

;;


let ccs =
  if (!process <> "") then
    CCS.parse (!process)
  else if (!go_file <> "") then (
    let (pipe_out, pipe_in) = Unix.pipe () in
    let pipe_out_channel = Unix.in_channel_of_descr pipe_out in
    Unix.set_close_on_exec pipe_out;
    let migoinferPID = Unix.create_process "migoinfer" (Array.of_list ["migoinfer"; !go_file]) Unix.stdin pipe_in Unix.stderr in
    Unix.close pipe_in;
    ignore (Unix.waitpid [] migoinferPID);
    let b = Buffer.create 16 in
    ignore (
      try
        while true; do
          Buffer.add_channel b pipe_out_channel 16;
        done;
      with End_of_file ->
        ()
    );
    (MiGo_to_CCS.migo_to_ccs (MiGo.parse (Buffer.contents b)))

  ) else if (!migo <> "") then (
    (MiGo_to_CCS.migo_to_ccs (MiGo.parse_file !migo))
  ) else (
    Format.printf "%s\n" Cmd.usage_msg;
    raise (Arg.Bad ("Give a CCS process or a MiGo file"))
  )
  in
ignore (main Format.std_formatter ccs)