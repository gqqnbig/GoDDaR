(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

let usage_msg = "Usage: ./dlock [-v | -s | -ds] <file1>"
let verbose = ref false
let simplified = ref false
let ds = ref 0

let speclist =
    [("-v", Arg.Set verbose, "Output debug information");
     ("-s", Arg.Set simplified, "Output a simpler representation of the process");
     ("-ds", Arg.Int (fun i -> if (i!=1 && i!= 2) then raise (Arg.Bad ("Bad argument: Select deadlock resolution algorithm (1 or 2)")) else ds:=i), "Select deadlock resolution algorithm (1 or 2)");
     ("  ", Arg.Unit (fun () -> ()), "Output the verdict only")]

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage_msg