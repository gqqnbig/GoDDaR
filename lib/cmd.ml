(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

let usage_msg = "Usage: ./GoDDaR [-v | -s | -ds] [-p <process> | -m <MiGo file>]"
let verbose = ref false
let simplified = ref false
let ds = ref 0

let process = ref ""
let migo = ref ""

let speclist =
    [("-v", Arg.Set verbose, "Output debug information");
     ("-s", Arg.Set simplified, "Output a simpler representation of the process");
     ("-ds", Arg.Int (fun i -> if (i!=1 && i!= 2) then raise (Arg.Bad ("Bad argument: Select deadlock resolution algorithm (1 or 2)")) else ds:=i), "Select deadlock resolution algorithm (1 or 2)");
     ("-p", Arg.Set_string process, "Process this process");
     ("-m", Arg.Set_string migo, "Convert and process MiGo file")
    ]

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg