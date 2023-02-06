open Format
open Dlock
open Types


(* ------------------- COMMAND LINE -------------------- *)


let ccs_spec_list: (Arg.key * Arg.spec * Arg.doc) list = [
]
let migo_spec_list: (Arg.key * Arg.spec * Arg.doc) list = [
]
let go_spec_list: (Arg.key * Arg.spec * Arg.doc) list = [
  ("-patch", Arg.Set Cmd.patch, "Output patched Go code");
]
let global_spec_list: (Arg.key * Arg.spec * Arg.doc) list =
    [
     ("-v", Arg.Set Cmd.verbose, "Output extra information");
     ("-ds", Arg.Int (fun i -> if (i!=1 && i!= 2) then raise (Arg.Bad ("Bad argument: Select deadlock resolution algorithm (1 or 2)")) else Cmd.ds:=i), "Select deadlock resolution algorithm (1 or 2)");
     (* ("-a", Arg.Set all_etas, "Resolve all etas"); *)
    ]

let usage_msg = "Usage: ./GoDDaR [-v | -ds ] [ccs <process> | migo <MiGo file> | go [-patch] <Go file>]"

let spec_list = ref global_spec_list

let anon_func: string -> unit =
  fun s -> (
    match !Cmd.selected_mode with 
    | None ->
      (
        match s with 
        | "ccs" ->
          Cmd.selected_mode := Cmd.CCS({expression = (Option.None)});
          spec_list := !spec_list @ ccs_spec_list
        | "migo" ->
          Cmd.selected_mode := Cmd.MiGo({filename = (Option.None)});
          spec_list := !spec_list @ migo_spec_list
        | "go" ->
          Cmd.selected_mode := Cmd.Go({filename = (Option.None)});
          spec_list := !spec_list @ go_spec_list
        | _ -> raise (Arg.Bad ("Bad argument: " ^ s))
      )
    | Cmd.Go(options) ->   Cmd.selected_mode := Cmd.Go{options with filename = Some(s)}
    | Cmd.MiGo(options) -> Cmd.selected_mode := Cmd.MiGo{options with filename = Some(s)}
    | Cmd.CCS(options) ->  Cmd.selected_mode := Cmd.CCS{options with expression = Some(s)}
    
  )

let cmdParse = Arg.parse_dynamic spec_list anon_func usage_msg

;;
let ccs: (LambdaTagged.t * dependency list) =
  match !Cmd.selected_mode with
  | Cmd.None -> raise (Arg.Bad ("Please specify a subcommand"))
  | Cmd.CCS(options) -> (
    match options.expression with
    | None -> raise (Arg.Bad ("Please specify a CCS expression"))
    | Some(filename) -> (CCS.parse filename, [])
  )
  | Cmd.MiGo(options) -> (
    match options.filename with
    | None -> raise (Arg.Bad ("Please specify a MiGo filename"))
    | Some(filename) -> (MiGo_to_CCS.migo_to_ccs (MiGo.parse_file filename))
  )
  | Cmd.Go(options) -> (
    match options.filename with
    | None -> raise (Arg.Bad ("Please specify a Go program filename"))
    | Some(filename) -> 
      (
        let (pipe_out, pipe_in) = Unix.pipe () in
        let pipe_out_channel = Unix.in_channel_of_descr pipe_out in
        Unix.set_close_on_exec pipe_out;
        let migoinferPID = Unix.create_process "migoinfer" (Array.of_list ["migoinfer"; filename]) Unix.stdin pipe_in Unix.stderr in
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
      )
  )
in
ignore (Deadlock_detector.main Format.std_formatter ccs)