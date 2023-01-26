(* ------------------- COMMAND LINE ARGUMENTS -------------------- *)

(* Mode args *)

type mode =
  | None
  | Go of {filename: string option}
  | MiGo of {filename: string option}
  | CCS of {expression: string option}

let selected_mode: mode ref = ref None
(* Global args *)
let verbose = ref false
let ds = ref 0
let patch = ref false
let all_etas = ref false