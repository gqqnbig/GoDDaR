open Dlock;;

(* Format.printf "%s\n" (Sys.getcwd()); *)
Sys.chdir "data";
( Array.to_list (Sys.readdir "."))
|> List.filter (
  fun elem -> 
    (Sys.is_directory elem) && (String.get elem 0 <> '.')
) |> (
  List.sort compare
) |> List.iter (
  fun dir -> 
    let migo_file = (dir ^ "/main.migo" ) in
    Format.printf "%s\n" migo_file;
    let migo = MiGo.parse_file migo_file in
    MiGo_Types.print_migo_list Format.std_formatter migo;
    Format.printf "%s\n" (String.make 10 '=');
    (
      try 
        let ccs = (MiGo_to_CCS.migo_to_ccs migo) in
        Format.printf "%s\n" ccs;
        ignore (CCS.parse ccs);
      with
      | MiGo_to_CCS.Fail(reason) -> Format.printf "FAILED TO CONVERT: %s\n" reason;
      | _ -> Format.printf "NOT VALID CCS"
    );
    Format.printf "%s\n" (String.make 50 '=');
)