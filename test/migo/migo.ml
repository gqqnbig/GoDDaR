open Dlock;;

let readdir dir: string list = 
  Sys.readdir dir 
  |> Array.map (fun subdir -> (dir ^ "/" ^ subdir))
  |> Array.to_list
;;

(* Format.printf "%s\n" (Sys.getcwd()); *)
Sys.chdir "data";
[(readdir "simple"); (readdir "benchmark")]
|>  List.flatten
|>  List.filter (fun elem -> (Sys.is_directory elem) && (String.get elem 0 <> '.'))
|>  (List.sort compare)
|>  List.iter (
      fun dir -> 
        let migo_file = (dir ^ "/main.migo" ) in
        Format.printf "%s\n" migo_file;
        (
          try 
            let migo = MiGo.parse_file migo_file in
            MiGo_Types.print_migo_list Format.std_formatter migo;
            Format.printf "%s\n" (String.make 10 '=');
            (
              try 
                let ccs = (MiGo_to_CCS.migo_to_ccs migo) in
                Format.printf "%s\n" ccs;
                try 
                  ignore (CCS.parse ccs);
                with
                | _ -> Format.printf "NOT VALID CCS %s\n" ccs
              with
              | MiGo_to_CCS.Fail(reason) -> Format.printf "FAILED TO CONVERT: %s\n" reason;
            );
          with
          | _ -> Format.printf "FAILED TO PARSE MIGO\n"
        );
        Format.printf "%s\n" (String.make 50 '=');
    )