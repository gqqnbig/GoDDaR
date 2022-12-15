open Dlock
open Dlock.Types;;

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
        if Sys.file_exists migo_file then (
          (
            try 
              let fmt = Format.std_formatter in
              let migo = MiGo.parse_file migo_file in
              MiGo_Types.print_migo_list fmt migo;
              Format.printf "%s\n" (String.make 10 '=');
              (
                try 
                  let (ccs, deps) = (MiGo_to_CCS.migo_to_ccs migo) in
                  Lambda.print_lambda_simple fmt (Types.lambdaTaggedToLambda ccs);
                  Format.printf "\n";
                  if deps <> [] then (
                    Format.fprintf fmt "DEPS:\n"; 
                    List.iter (fun dep -> Format.fprintf fmt "%a\n" Types.print_dependecy dep) deps;
                  )
                with
                | MiGo_to_CCS.Fail(reason) -> Format.printf "FAILED TO CONVERT: %s\n" reason;
              );
            with
            | _ -> Format.printf "FAILED TO PARSE MIGO\n";
          );
          Format.printf "%s\n" (String.make 50 '=');
        );
    )