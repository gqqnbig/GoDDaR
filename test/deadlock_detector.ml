open Dlock
open Dlock.Printer
open Dlock.Types
open Format

module LambdaFlattenedSet = Set.Make(LambdaFlattened)

let bool_to_string b =
  match b with
  | true  -> "true "
  | false -> "false"

let lambdaFlattenedToProc l: proc =
  toProc (LambdaFlattened.lambdaFlattenedToLambda l)

let convert_res (passed_act_ver, deadlocked0, resolved0) =
  let deadlocked0_no_LNil = List.filter (fun l -> l <> LNil) deadlocked0 in
  (passed_act_ver,
   LambdaFlattenedSet.of_list (List.map LambdaFlattened.lambdaToLambdaFlattened deadlocked0_no_LNil),
   LambdaFlattenedSet.of_list (List.map LambdaFlattened.lambdaToLambdaFlattened resolved0  ))

let compare_res (passed_act_ver0, deadlocked0, resolved0) (passed_act_ver1, deadlocked1, resolved1): bool =
  passed_act_ver0 = passed_act_ver1 &&
  LambdaFlattenedSet.compare deadlocked0 deadlocked1 == 0 &&
  LambdaFlattenedSet.compare resolved0 resolved1 == 0 

let print_res_summary fmt exp (passed_act_ver0, deadlocked0, resolved0) : unit =
  let len_deadlocked = (LambdaFlattenedSet.cardinal deadlocked0) in
  fprintf fmt "act_ver: %s #deadlock: %i, exp: %s" (bool_to_string passed_act_ver0) len_deadlocked exp;
  if len_deadlocked > 0 then (
    fprintf fmt ", resolved: ";
    let [@warning "-8"] hd::_ = (LambdaFlattenedSet.elements resolved0) in
    print_proc_simple fmt (lambdaFlattenedToProc hd);
  );
  fprintf fmt "\n"

let print_res fmt (passed_act_ver0, deadlocked0, resolved0) (passed_act_ver1, deadlocked1, resolved1): unit =
  let deadlocked0 = LambdaFlattenedSet.elements deadlocked0 in
  let deadlocked1 = LambdaFlattenedSet.elements deadlocked1 in
  let resolved0   = LambdaFlattenedSet.elements resolved0 in
  let resolved1   = LambdaFlattenedSet.elements resolved1 in
  fprintf fmt " passed_act_ver: 0: %b, 1: %b\n" passed_act_ver0 passed_act_ver1;
  fprintf fmt " Deadlocked: \n";
  fprintf fmt "  0\n";
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked0;
  fprintf fmt "  1\n";
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked1;
  fprintf fmt " Solved: \n";
  fprintf fmt "  0\n";
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") resolved0;
  fprintf fmt "  1\n";
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") resolved1

let test fmt (exp: string) = 
  let proc = CCS.parse exp in
  let result0 = convert_res (Deadlock_detector.main  null_fmt proc) in
  let result1 = convert_res (Deadlock_detector2.main null_fmt proc) in
  if compare_res result0 result1 then (
    fprintf fmt "PASSED:";
    print_res_summary fmt exp result0
  ) else (
    fprintf fmt "FAILED: %s\n" exp;
    print_res fmt result0 result1
  );
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
)