open Dlock
open Dlock.Auxfunctions
open Dlock.Types
open Dlock.CCS
open Dlock.Printer

let test exp_string ((res0: (eta LambdaFlattened.lambda_flattened * eta list) list), has_miss_acts0)
                    ((res1: (eta LambdaFlattened.lambda_flattened * eta list) list), has_miss_acts1) = 
  (* Check if lambdas are equal, if the unbalanced actions are the same, even if reordered, and if
    the [has_miss_acts] returns the same *)
  let equal_res (res0: (eta LambdaFlattened.lambda_flattened * eta list) list) (res1: (eta LambdaFlattened.lambda_flattened * eta list) list) = 
    (List.compare_lengths res0 res1) = 0
    && let res0_sorted = List.sort (fun (lf1, _) (lf2, _) -> compare lf1 lf2) res0 in
       let res1_sorted = List.sort (fun (lf1, _) (lf2, _) -> compare lf1 lf2) res1 in
       List.equal (=) res0_sorted res1_sorted
    && (has_miss_acts0 = has_miss_acts1)
  in
    let equal = equal_res res0 res1 in
    if equal then (
      let num_unbalenced_executions = List.length (
          (List.filter (fun (_, eta_list) -> List.length eta_list <> 0)) res0
        )
      in
        Format.fprintf fmt "PASS: %i %i %s\n" (List.length res0) num_unbalenced_executions exp_string;
    ) else (
        Format.fprintf fmt "FAIL: %s\n" exp_string;
        List.iteri (fun res_n (res, has_miss_acts )-> 
          Format.fprintf fmt " RES%i:\n" res_n;
          List.iteri (fun i (lambda_flattened, eta_list) ->
            let lambda = LambdaFlattened.lambdaFlattenedToLambda lambda_flattened in
            Format.fprintf fmt "  LAMBDA %i: " i;
            print_proc_simple fmt (toProc lambda);
            Format.fprintf fmt "\n  ETAS %i: " i;
            print_etalist fmt eta_list;
            Format.fprintf fmt "\n  HAS MISSING ACTS: %b" has_miss_acts
            ) res;
          Format.fprintf fmt "\n"
        ) [(res0, has_miss_acts0); (res1, has_miss_acts1)];
      );
      equal
;;

Format.fprintf fmt "MAIN_ACT_VERIFIER:\n";
let test_pass = ref true in
  List.iter (fun exp -> 
    let lambda = toLambda (parse exp) in
    test_pass := (
      test 
        exp
        (
          let res = (Main_act_verifier_orig.main_act_verifier lambda) in
            (
              (List.map (fun (lambda, eta_list) -> ( (LambdaFlattened.lambdaToLambdaFlattened lambda), List.sort compare eta_list)) res),
              Main_act_verifier_orig.has_miss_acts res
            )
        )
        (
          let res = List.rev (Main_act_verifier.main_act_verifier lambda) in
            (
              (List.map (fun (lambda, eta_list) -> ( LambdaFlattened.lambdaToLambdaFlattened lambda, List.sort compare eta_list)) res),
              Main_act_verifier.has_miss_acts res
            )
        )
      ) && !test_pass
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