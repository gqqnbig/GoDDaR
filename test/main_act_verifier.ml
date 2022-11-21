open Dlock
open Dlock.Auxfunctions
open Dlock.Types
open Dlock.CCS
open Dlock.Printer

let fmt = Format.std_formatter

let test exp_string ((res0: (eta LambdaC.lambdaC * eta list) list), has_miss_acts0) = 
  Format.fprintf fmt "%s\n" exp_string;
  List.iteri (fun res_n (res, has_miss_acts )-> 
    Format.fprintf fmt " RES%i:\n" res_n;
    List.iteri (fun i (lambda_flattened, eta_list) ->
      let lambda = LambdaC.lambdaCToLambda lambda_flattened in
      Format.fprintf fmt "  LAMBDA %i: " i;
      print_lambda_simple fmt lambda;
      Format.fprintf fmt "\n  ETAS %i: " i;
      print_etalist fmt eta_list;
      Format.fprintf fmt "\n  HAS MISSING ACTS: %b" has_miss_acts
      ) res;
    Format.fprintf fmt "\n"
  ) [(res0, has_miss_acts0)];
;;

Format.fprintf fmt "MAIN_ACT_VERIFIER:\n";
  List.iter (fun exp -> 
    let lambda = lambdaTaggedToLambda (parse exp) in
      test 
        exp
        (
          let result = List.rev (Main_act_verifier.main_act_verifier lambda) in
            (
              (List.map (fun (lambda, eta_list) -> ( LambdaC.lambdaToLambdaC lambda, List.sort compare eta_list)) result),
              Main_act_verifier.has_miss_acts result
            )
        )
  ) [
    "a!.a?.0 || (b!.x?.0 + (b?.0 + c?.0) )";
    "a!.a?.0 || (b!.0 + (b?.0 + c?.0) )";
    "a!.0 || (b!.0 + (b?.0 + c?.0) )";
    "a!.0 || (b?.0 + c?.0)";
    "(a!.0 + a?.0) || (b?.0 + c?.0)";
    "a!.0 || a?.0";
    "a!.b?.0 || a?.b!.0";
    "(a!.0 + b!.0) || (a?.0 + b?.0)";
    "(a!.0 || b!.0) || (a?.0 + b?.0)";
    "(a!.0 || b!.0) || (a?.b?.0 + b?.a?.0)";
    "a!.0 || (a?.0 + 0)";
    "(a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0)";
    "a?.(c?.0 + d?.0) || a!.e!.0";
    (* "a?.(a!.0 + a!.0)"; Crashes original*)
  ]