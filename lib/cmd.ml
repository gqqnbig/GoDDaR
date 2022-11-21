(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

let usage_msg = "Usage: ./GoDDaR [-v | -s | -ds] [-p <process> | -m <MiGo file>]"
let verbose = ref false
let simplified = ref false
let ds = ref 0