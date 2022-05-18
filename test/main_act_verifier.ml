open Dlock
open Dlock.Auxfunctions
open Dlock.Types
open Dlock.CCS
open Dlock.Printer

let test exp_string (res0, has_miss_acts0) (res1, has_miss_acts1)= 
  (* Check if lambdas are equal, if the unbalanced actions are the same, even if reordered, and if
    the [has_miss_acts] returns the same *)
  let equal_res res0 res1 = 
    (List.compare_lengths res0 res1) = 0
    &&  (List.equal (fun (l1, el1) (l2, el2) -> (
        List.equal (=) (List.sort compare l1) (List.sort compare l2)
        && List.equal (=) (List.sort compare el1) (List.sort compare el2)
        (* && SetChar.equal (SetChar.of_list el1) (SetChar.of_list el2) *)
      )) res0 res1)
    && (has_miss_acts0 = has_miss_acts1)
  in
    if equal_res res0 res1 then (
      let num_unbalenced_executions = List.length (
          (List.filter (fun (_, eta_list) -> List.length eta_list <> 0)) res0
        )
      in
        Format.fprintf fmt "PASS: %i %i %s\n" (List.length res0) num_unbalenced_executions exp_string
    ) else (
        Format.fprintf fmt "FAIL: %s\n" exp_string;
        List.iteri (fun res_n (res, has_miss_acts )-> 
          Format.fprintf fmt "RES%i:\n" res_n;
          List.iteri (fun i (lambda_list, eta_list) ->
            Format.fprintf fmt "LAMBDA %i: " i;
            print_lambdalist fmt lambda_list;
            Format.fprintf fmt "\nETAS %i: " i;
            print_etalist fmt eta_list;
            Format.fprintf fmt "\nHAS MISSING ACTS: %b\n" has_miss_acts
            ) res;
          Format.fprintf fmt "\n"
        ) [(res0, has_miss_acts0); (res1, has_miss_acts1)];
        Format.fprintf fmt "\n\n";
      )
;;

Format.fprintf fmt "MAIN_ACT_VERIFIER:\n";
List.iter (fun exp -> 
  let lambda = toLambda (parse exp) in
  test 
    exp
    begin
      let res = (Main_act_verifier_orig.main_act_verifier lambda) in
        (
          (List.map (fun (lambda, eta_list) -> ( List.filter (fun l -> l<>LNil) (List.rev (lparToList lambda)), eta_list)) res),
          Main_act_verifier_orig.has_miss_acts res
        )
    end
    begin
      let res = List.rev (Main_act_verifier.main_act_verifier lambda) in
        (
          (List.map (fun (lambda, eta_list) -> ( List.filter (fun l -> l<>LNil) (lparToList lambda), eta_list)) res),
          Main_act_verifier.has_miss_acts res
        )
    end
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