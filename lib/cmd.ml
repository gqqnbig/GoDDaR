(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

let usage_msg = "Usage: ./GoDDaR [-v | -ds | --go] [-p <process> | -m <MiGo file>]"
let verbose = ref false
let ds = ref 0

let go = ref false