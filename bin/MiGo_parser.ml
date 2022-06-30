open Dlock

let migo_str = ref ""
let migo_file = ref ""

let speclist =
    [
     ("-m", Arg.Set_string migo_str, "Migo");
     ("-f", Arg.Set_string migo_file, "Migo file");
    ]

let cmdParse = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) ""
;;

let migo =
  if !migo_str <> "" then
    Some(MiGo.parse (!migo_str))
  else if !migo_file <> "" then
    Some((MiGo.parse_file (!migo_file)))
  else None
in
  Option.iter (
    fun migo -> 
      MiGo_Types.print_migo_list Format.std_formatter migo;
      Format.printf "\n\n\n%s\n" (MiGo_to_CCS.migo_to_ccs migo);
  ) migo;