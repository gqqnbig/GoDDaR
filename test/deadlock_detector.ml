open Dlock
open Dlock.Types
open Format

module LambdaCSet = Set.Make(LambdaC)

let bool_to_string b =
  match b with
  | true  -> "true "
  | false -> "false"

let convert_res (passed_act_ver, deadlocked0, resolved0) :(bool * LambdaCSet.t * LambdaC.t) =
  let deadlocked0_no_LNil = List.filter (fun l -> l <> Lambda.LNil) deadlocked0 in
  (passed_act_ver,
   LambdaCSet.of_list (List.map LambdaC.lambdaToLambdaC deadlocked0_no_LNil),
   LambdaC.lambdaToLambdaC resolved0 )

type compare_failure =
  | ACT_VER
  | DEADLOCK_LIST
  | RESOLVED
let compare_failure_to_string (failure: compare_failure) = 
  match failure with
  | ACT_VER -> "ACT_VER"
  | DEADLOCK_LIST -> "DEADLOCK_LIST"
  | RESOLVED -> "RESOLVED"

let compare_res (passed_act_ver0, deadlocked0, resolved0) (passed_act_ver1, deadlocked1, resolved1): compare_failure list =
  (if passed_act_ver0 = passed_act_ver1 then [] else [ACT_VER]) @
  (if LambdaCSet.compare deadlocked0 deadlocked1 == 0 then [] else [DEADLOCK_LIST]) @
  (if LambdaCSet.compare resolved0 resolved1 == 0 then [] else [RESOLVED])

let print_res_summary fmt exp (passed_act_ver0, deadlocked0, resolved0) : unit =
  let len_deadlocked = (LambdaCSet.cardinal deadlocked0) in
  fprintf fmt "act_ver: %s #deadlock: %i, exp: %s" (bool_to_string passed_act_ver0) len_deadlocked exp;
  if len_deadlocked > 0 then (
    fprintf fmt ", resolved: ";
    let [@warning "-8"] hd::_ = (LambdaCSet.elements resolved0) in
    Lambda.print_lambda_simple fmt (LambdaC.lambdaCToLambda hd);
  );
  fprintf fmt "\n"

let print_res fmt (passed_act_ver, deadlocked, resolved) (passed_act_ver2, deadlocked22, resolved2): unit =
  let deadlocked = LambdaCSet.elements deadlocked in
  (
    fprintf fmt " passed_act_ver: %b\n" passed_act_ver;
    fprintf fmt " Deadlocked:\n";
    List.iter (fun l -> fprintf fmt "   "; Lambda.print_lambda_simple fmt (LambdaC.lambdaCToLambda l); fprintf fmt "\n") deadlocked;
    fprintf fmt " Solved:\n";
    fprintf fmt "   %a\n" Lambda.print_lambda_simple (LambdaC.lambdaCToLambda resolved) ;
    fprintf fmt " Solved2:\n";
    fprintf fmt "   %a\n" Lambda.print_lambda_simple (LambdaC.lambdaCToLambda resolved2) ;
  )

let test fmt (exp: string) = 
  fprintf fmt "%s\n" exp;
  pp_print_flush fmt ();
  let proc = CCS.parse exp in
  Cmd.ds := 1;
  let result = convert_res (Deadlock_detector.main  null_fmt (proc, [])) in
  Cmd.ds := 2;
  let result2 = convert_res (Deadlock_detector.main  null_fmt (proc, [])) in
  (
    print_res fmt result result2
  )

;;


let fmt = Format.std_formatter in (
  fprintf fmt "DEADLOCK_DETECTOR:\n";
  test fmt "a!.a?.0 || (b!.x?.0 + (b?.0 + c?.0) )";
  test fmt "a!.a?.0 || (b!.0 + (b?.0 + c?.0) )";
  test fmt "a!.0 || (b!.0 + (b?.0 + c?.0) )";
  test fmt "a!.0 || (b?.0 + c?.0)";
  test fmt "(a!.0 + a?.0) || (b?.0 + c?.0)";
  test fmt "a!.0 || a?.0";
  test fmt "a!.b?.0 || a?.b!.0";
  test fmt "(a!.0 + b!.0) || (a?.0 + b?.0)";
  test fmt "(a!.0 || b!.0) || (a?.0 + b?.0)";
  test fmt "(a!.0 || b!.0) || (a?.b?.0 + b?.a?.0)";
  test fmt "a!.0 || (a?.0 + 0)";
  test fmt "(a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0)";
  test fmt "a?.(c?.0 + d?.0) || a!.e!.0";
  test fmt "(a!.a?.0 || b?.b!.0) + c!.c?.0";
  test fmt "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0";
  test fmt "a!.0 || (b!.b?.a?.0 + a?.0)";

  (* Taken from test_file.txt*)
  test fmt "a?.0";
  test fmt "a?.a!.0";
  test fmt "0 || 0";
  test fmt "a?.0 || a!.0";
  test fmt "a?.0 || b!.0";
  test fmt "a?.b!.0 || a!.b?.0";
  test fmt "a?.b!.c?.0 || a!.b?.c!.0";
  test fmt "a?.b!.c?.0 || a!.c!.b?.0";
  test fmt "a?.b?.0 || a!.b!.c!.0";
  test fmt "a?.0 || a!.b!.c!.0";
  test fmt "0 || a!.b!.c!.0";
  test fmt "a!.(0 || 0) || b!.0";
  test fmt "(a!.b?.c?.0 || a?.b!.0) || c!.0";
  test fmt "a!.b?.0 || a?.0 || a?.a!.b!.0";
  test fmt "a!.0 || a?.0 || a?.a!.b!.0";
  test fmt "b!.a!.0 || a?.0 || b?.0";
  test fmt "0 || a?.0 || a!.0";
  test fmt "a?.0 || 0 || b!.0";
  test fmt "a?.0 || 0 || a?.0";
  test fmt "a?.a?.0 || 0 || a?.b!.0";
  test fmt "a?.a?.0 || b?.c?.0 || a?.b!.0";
  test fmt "a?.(b!.0 || c?.d?.0) || b?.0 || a!.(c!.0 || d!.0)";
  test fmt "0 || 0 || 0";
  test fmt "b!.(0 || 0) || b?.0 || b?.b!.(a!.0 || a?.0)";
  test fmt "a!.0 || b!.0 || a?.0 || b?.0";
  test fmt "a!.(b!.(c!.0 || d?.0) || b?.0) || a?.c?.d!.0";
  test fmt "a!.(b!.(c!.0 || d?.0) || b?.0) || a?.(c?.0 || e!.0)";
  test fmt "a!.(a!.(a?.0 || a?.0 ) || a!.0) || a?.(a?.0 || a?.0 || a!.0)";
  test fmt "a!.(a!.(b?.0 || c?.0 )) || a?.(a?.0 || b!.0 || c!.0)";
  (* test fmt "a!.(a!.(b?.0 || c?.0 ) || d!.0) || a?.(a?.0 || b!.0 || c!.0 || d?.0)"; *)
  test fmt "a!.b?.0 || c?.d?.a!.0 || c!.a?.0 || b!.d!.a?.0";
  (* test fmt "a!.a?.0 || a?.a?.a!.0 || a!.a?.0 || a!.a!.a?.0 || a!.a?.0";*)
  test fmt "(a!.0 || a?.0) + (b!.0 || b?.0)";
  test fmt "a!.(b?.0 + 0) || a?.b!.0";
  test fmt "a!.0 + b!.0 + c!.0";
  (* test fmt "(a!.0 || a?.0) + (b!.0 || b?.0) + (c!.0 || c?.0)"; *)
  test fmt "(a!.0 || a?.0) + (a!.0 || a?.b?.(c?.0 + d?.0) || b!.d!.0 )";
  test fmt "(a!.0 || a?.0) + (a!.0 || a?.b?.(c?.0 + d?.0) || c!.b!.d!.0 )";
  (* test fmt "a!.b?.d?.0 || a!.a?.0 || b!.c?.0 || c!.a?.0 || d!.(e!.0 || e?.0)"; *)
  test fmt "b?.((d?.0 || d!.0) + (c!.0 || c?.0)) || a?.b!.a!.0";
  (* test fmt "d!.a?.(b?.0 || e?.d!.0) || d?.e!.d?.0 || b!.a!.0"; *)
  test fmt "a!.0 || a?.b!.0 || a?.b!.0 || b?.b?.0";
  test fmt "a!.a!.0 || a?.b?.0 || a?.0 + b!.0";
  test fmt "a!.0 || b!.0 || a?.c!.0 + b?.c!.0 || c?.0";
  test fmt "a!.0 || b!.0 || a?.c!.(a?.c!.0 + b?.c!.0) + b?.c!.(a?.c!.0 + b?.c!.0) || c?.c?.0";
  test fmt "a!.0 || b!.0 || a?.c!.b?.c!.0 + b?.c!.a?.c!.0 || c?.c?.0";
  test fmt "a!.0 || b!.0 || (a?.c!.0 + b!.c!.0 + 0) || (c?.0 + 0)";
  test fmt "a!.0 || b!.0 || a?.c!.(a?.c!.0 + b?.c!.0 + 0) + b?.c!.(a?.c!.0 + b?.c!.0 + 0) + 0 || c?.c?.0";
  test fmt "b!.b!.0 + b!.0 || a?.0 + b?.b?.0";
  test fmt "(b!.(b!.0 + a!.0) + a!.(b!.0 + a!.0)) || (a?.0 + b?.b?.0)";
  test fmt "b?.a!.0 + c!.0 || c?.0 + b!.a!.0 || a?.a?.0";
  test fmt "b?.a!.0 + c!.(b?.a!.0 + c!.0) || c?.(c?.0 + b!.a!.0) + b!.a!.0 || a?.a?.0";
  (* test fmt "a!.a?.0 || b!.b?.0 || c!.c?.0 || a?.(b?.a!.b!.0 + a!.0) + b?.(a?.b!.a!.0 + b!.0) || b?.(c?.b!.c!.0 + b!.0) + c?.(b?.c!.b!.0 + c!.0) || c?.(a?.c!.a!.0 + c!.0) + a?.(c?.a!.c!.0 + a!.0)"; *)
  test fmt "a?.0 + b?.0 || a?.0 + b?.0 || a!.(a!.0 + b!.0) + b!.0";
  test fmt "a?.a!.0 + b!.a!.0 + c?.a!.0 + a!.0";
  test fmt "b?.b!.b?.b!.0 || b?.b!.0";
  test fmt "b!.b?.b!.b?.0 || b!.b?.0";
  test fmt "(b!.a!.0 + b?.a!.0) + a?.0";
  test fmt "(b!.0 + a!.0) || (b?.0 + a?.0)";
  test fmt "a!.a!.a!.b!.0 || a?.a?.a?.b?.0";
  test fmt "(a!.0 + (b?.a!.0 || b!.0)) || a?.0";
  (* test fmt "a?.0 || b?.a!.0 || c?.b!.0 || d?.c!.0 || e?.d!.0 || e!.0"; *)
  test fmt "a!.0 || b!.0 || (a?.c!.b?.c!.0 + b?.c!.a?.c!.0) || c?.c?.0";
  test fmt "a!.0 || a?.b!.0 || a?.b!.0 || b?.b?.0";
  test fmt "a!.0 || a?.b!.0 || b?.0";
  test fmt "a?.0 || b?.c?.a!.0 || b!.0 || c!.0";
  test fmt "a?.0 || a!.0";
  test fmt "a!.0 || b!.0 || (a?.(a?.0 + b?.0) + b?.(a?.0 + b?.0))";
  test fmt "a!.0 || b!.0 || (a?.b?.0 + b?.a?.0)";
  test fmt "a?.b!.0 || a!.0 || a!.0 || b?.b?.0";
  test fmt "a!.a!.a!.0 || a?.a?.a?.a?.0";
  test fmt "a!.a!.a!.0 || a?.a?.a?.0";
  test fmt "a!.0 || b!.0 || a?.c!.0 || b?.c!.0 || c?.d?.0 || a?.d!.0 || b?.d!.0";
  test fmt "a!.0 || b!.0 ||(a?.0 + b?.0)";
  test fmt "a!.a!.a!.a!.a!.a!.0 || a?.b?.c?.0 || a?.b!.a?.a?.b!.0 || b?.c!.0";
  test fmt "a!.0 || a?.0";

  test fmt "a!.0 & a?.0";
  test fmt "a!.a?.0 & a?.a!.0";
  test fmt "a!.0 || ( a?.0 & a?.0)";
  test fmt "a!.0 || ( (a?.a?.0 || a!.0) & a?.0)";
  test fmt "a!.0 || ( (a?.a?.0 || a!.b?.b!.0) & a?.0)";
  (* Can only syncronize with one path of the LOrE, but no deadlock *)
  test fmt "a?.0 || (c?.c!.a!.0 & a!.0)";
  (* Can't syncronize with any path of the LOrE, deadlock *)
  test fmt "a?.0 || (c?.c!.a!.0 & c?.c!.a!.0)";
  (* Reduce inside LOrE once before syncronizing *)
  test fmt "a?.0 || ((c?.a!.0 || c!.0) & b!.b?.a!.0)";
  (* LOrI inside LOrE, but no deadlock *)
  test fmt "a!.0 || (a?.0 & (a?.0 + b!.b?.a?.0))";
  (* LOrI inside LOrE, but deadlock *)
  test fmt "a!.0 || (a?.0 & (a?.0 + a?.b!.b?.0))";
  (* Two deadlocks *)
  test fmt "a!.0 || (a?.0 & (a?.b!.b?.0 + a?.c!.c?.0))";
  test fmt "(a!.0 + b!.0) || (b?.0 & a?.0)";
  test fmt "(b?.0 & a?.0) || (a!.0 + b!.0)";
)