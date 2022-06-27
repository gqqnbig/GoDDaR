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

let convert_res_manual (passed_act_ver, deadlocked0, resolved0) =
  convert_res (passed_act_ver,
   List.map (fun exp -> toLambda (CCS.parse exp)) deadlocked0,
   List.map (fun exp -> toLambda (CCS.parse exp)) resolved0)

type compare_failure =
  | ACT_VER
  | DEADLOCK_LIST
  | RESOLVED
let string_compare_failure (failure: compare_failure) = 
  match failure with
  | ACT_VER -> "ACT_VER"
  | DEADLOCK_LIST -> "DEADLOCK_LIST"
  | RESOLVED -> "RESOLVED"

let compare_res (passed_act_ver0, deadlocked0, resolved0) (passed_act_ver1, deadlocked1, resolved1): compare_failure list =
  (if passed_act_ver0 = passed_act_ver1 then [] else [ACT_VER]) @
  (if LambdaFlattenedSet.compare deadlocked0 deadlocked1 == 0 then [] else [DEADLOCK_LIST]) @
  (if LambdaFlattenedSet.compare resolved0 resolved1 == 0 then [] else [RESOLVED])

let print_res_summary fmt exp (passed_act_ver0, deadlocked0, resolved0) : unit =
  let len_deadlocked = (LambdaFlattenedSet.cardinal deadlocked0) in
  fprintf fmt "act_ver: %s #deadlock: %i, exp: %s" (bool_to_string passed_act_ver0) len_deadlocked exp;
  if len_deadlocked > 0 then (
    fprintf fmt ", resolved: ";
    let [@warning "-8"] hd::_ = (LambdaFlattenedSet.elements resolved0) in
    print_proc_simple fmt (lambdaFlattenedToProc hd);
  );
  fprintf fmt "\n"

let print_res fmt (name0, (passed_act_ver0, deadlocked0, resolved0)) (name1, (passed_act_ver1, deadlocked1, resolved1)): unit =
  let deadlocked0 = LambdaFlattenedSet.elements deadlocked0 in
  let deadlocked1 = LambdaFlattenedSet.elements deadlocked1 in
  let resolved0   = LambdaFlattenedSet.elements resolved0 in
  let resolved1   = LambdaFlattenedSet.elements resolved1 in
  fprintf fmt " passed_act_ver: 0: %b, 1: %b\n" passed_act_ver0 passed_act_ver1;
  fprintf fmt " Deadlocked: \n";
  fprintf fmt "  %s:\n" name0;
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked0;
  fprintf fmt "  %s:\n" name1;
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked1;
  fprintf fmt " Solved: \n";
  fprintf fmt "  %s:\n" name0;
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") resolved0;
  fprintf fmt "  %s:\n" name1;
  List.iter (fun l -> fprintf fmt "   "; print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") resolved1

let test_manual fmt exp (name0, result0) (name1, result1) =
  let compare_failures = compare_res result0 result1 in
  if compare_failures = [] then (
    fprintf fmt "PASSED: ";
    print_res_summary fmt exp result0
  ) else (
    fprintf fmt "FAILED: %s: " exp;
    List.map string_compare_failure compare_failures
      |> List.iter (fun s -> fprintf fmt "%s " s);
    fprintf fmt "\n";
    print_res fmt (name0, result0) (name1, result1)
  )

let test fmt (exp: string) = 
  let proc = CCS.parse exp in
  let result0 = convert_res (Deadlock_detector_orig.main  null_fmt proc) in
  let result1 = convert_res (Deadlock_detector.main null_fmt proc) in
  test_manual fmt exp ("orig", result0) ("mine", result1)

let test_one fmt (exp: string) result1 = 
  let proc = CCS.parse exp in
  let result0 = convert_res (Deadlock_detector.main  null_fmt proc) in
  let result1 = convert_res_manual result1 in
  test_manual fmt exp ("mine", result0) ("expected", result1)
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
  test fmt "a?.a!.0 ";
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
  test fmt "(a!.0 + (b?.a!.0 || b!.0)) || a?.0 ";
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

  test_one fmt "a!.0 & a?.0" (false, [], []) ;
  test_one fmt "a!.a?.0 & a?.a!.0" (true, ["a!.a?.0 & a?.a!.0"], ["(a!.0 || a?.0) & (a?.0 || a!.0)"]) ;
  test_one fmt "a!.0 || ( a?.0 & a?.0)" (true, [], ["a!.0 || ( a?.0 & a?.0)"]) ;
  test_one fmt "a!.0 || ( (a?.a?.0 || a!.0) & a?.0)" (true, [], ["a!.0 || ( (a?.a?.0 || a!.0) & a?.0)"]) ;
  test_one fmt "a!.0 || ( (a?.a?.0 || a!.b?.b!.0) & a?.0)" (true, ["b?.b!.0"], ["a!.0 || ((a?.a?.0 || a!.(b?.0 || b!.0)) & a?.0)"]) ;
  (* Can only syncronize with one path of the LOrE, but no deadlock *)
  test_one fmt "a?.0 || (c?.c!.a!.0 & a!.0)" (true, [], ["a?.0 || (c?.c!.a!.0 & a!.0)"]) ;
  (* Can't syncronize with any path of the LOrE, deadlock *)
  test_one fmt "a?.0 || (c?.c!.a!.0 & c?.c!.a!.0)" (true, ["a?.0 || (c?.c!.a!.0 & c?.c!.a!.0)"], ["(a?.0 || ((c?.0 || c!.a!.0) & (c?.0 || c!.a!.0)))"]) ;
  (* Reduce inside LOrE once before syncronizing *)
  test_one fmt "a?.0 || ((c?.a!.0 || c!.0) & b!.b?.a!.0)" (true, [], ["a?.0 || ((c?.a!.0 || c!.0) & b!.b?.a!.0)"]) ;
  (* LOrI inside LOrE, but no deadlock *)
  test_one fmt "a!.0 || (a?.0 & (a?.0 + b!.b?.a?.0))" (true, [], ["a!.0 || (a?.0 & (a?.0 + b!.b?.a?.0))"]) ;
  (* LOrI inside LOrE, but deadlock *)
  test_one fmt "a!.0 || (a?.0 & (a?.0 + a?.b!.b?.0))" (true, ["b!.b?.0"], ["(a!.0 || (a?.0 & (a?.0 + a?.(b!.0 || b?.0))))"]) ;
  (* Two deadlocks *)
  test_one fmt "a!.0 || (a?.0 & (a?.b!.b?.0 + a?.c!.c?.0))" (true, ["b!.b?.0"; "c!.c?.0"], ["a!.0 || (a?.0 & (a?.(b!.0 || b?.0) + a?.(c!.0 || c?.0)))"]) ;
)