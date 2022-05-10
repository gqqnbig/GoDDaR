open Dlock
open Dlock.Auxfunctions
open Dlock.Types
open Dlock.Types.Eta
open Dlock.CCS
open Dlock.Printer

module SetChar = Set.Make(Eta)

let test exp_string res0 res1 = 
  (* Check if lambdas are equal, and if the unbalanced actions are the same, even if reorderd*)
  let equal_res res0 res1 = 
    (List.compare_lengths res0 res1) = 0 &&
      List.equal (fun (l1, el1) (l2, el2) -> (
        l1 = l2 && SetChar.equal (SetChar.of_list el1) (SetChar.of_list el2)
      )) res0 res1
  in
    if equal_res res0 res1 then (
      let num_unbalenced_executions = List.length (
          (List.filter (fun (_, eta_list) -> List.length eta_list <> 0)) res0
        )
      in
        Format.fprintf fmt "PASS: %i %i %s\n" (List.length res0) num_unbalenced_executions exp_string
    ) else (
        Format.fprintf fmt "FAIL:\n";
        List.iteri (fun res_n res -> 
          Format.fprintf fmt "RES%i:\n" res_n;
          List.iteri (fun i (lambda_list, eta_list) ->
            Format.fprintf fmt "LAMBDA %i: " i;
            print_lambdalist fmt lambda_list;
            Format.fprintf fmt "\nETAS %i: " i;
            print_etalist fmt eta_list;
            Format.fprintf fmt "\n"
            ) res;
          Format.fprintf fmt "\n"
        ) [res0; res1];
        Format.fprintf fmt "\n\n";
      )
;;

Format.fprintf fmt "MAIN_ACT_VERIFIER:\n";
List.iter (fun exp -> 
  let lambda = toLambda (parse exp) in
  test 
    exp
    (List.map (fun (lambda, eta_list) -> List.rev (lparToList lambda), eta_list) (Main_act_verifier_orig.main_act_verifier lambda))
    (List.map (fun (lambda, eta_list) -> (
        List.filter (fun l -> l<>LNil) (lparToList lambda), eta_list
      )) (List.rev (Main_act_verifier.main_act_verifier lambda)))
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
]