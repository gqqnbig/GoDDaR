open Dlock
open Dlock.Printer
open Dlock.Types
open Format

module LambdaFlattenedSet = Set.Make(LambdaFlattened)

let fmt = Format.std_formatter
let null_fmt = Format.make_formatter ( fun _ _ _ -> () ) (fun _ -> ())

let lambdaFlattenedToProc l: proc =
  toProc (LambdaFlattened.lambdaFlattenedToLambda l)

let conv_res (deadlocked0, solved0) =
  ( LambdaFlattenedSet.of_list (List.map LambdaFlattened.lambdaToLambdaFlattened deadlocked0),
    LambdaFlattenedSet.of_list (List.map LambdaFlattened.lambdaToLambdaFlattened solved0    ))

let compare_res (deadlocked0, solved0) (deadlocked1, solved1): bool =
  LambdaFlattenedSet.compare deadlocked0 deadlocked1 == 0 &&
  LambdaFlattenedSet.compare solved0 solved1 == 0 

let print_res (deadlocked0, solved0) (deadlocked1, solved1): unit =
  let deadlocked0 = List.of_seq (LambdaFlattenedSet.to_seq deadlocked0) in
  let deadlocked1 = List.of_seq (LambdaFlattenedSet.to_seq deadlocked1) in
  let solved0 = List.of_seq (LambdaFlattenedSet.to_seq solved0) in
  let solved1 = List.of_seq (LambdaFlattenedSet.to_seq solved1) in
  fprintf fmt "Deadlocked: \n0\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked0;
  fprintf fmt "1\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") deadlocked1;
  fprintf fmt "Solved: \n0\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") solved0;
  fprintf fmt "1\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); fprintf fmt "\n") solved1

let test (exp: string) = 
  fprintf fmt "DEADLOCK_DETECTOR:\n";
  let proc = CCS.parse exp in
  let result0 = conv_res (Deadlock_detector.main  null_fmt proc) in
  let result1 = conv_res (Deadlock_detector2.main null_fmt proc) in
  if compare_res result0 result1 then
    fprintf fmt "PASSED: %s\n" exp
  else (
    fprintf fmt "FAILED: %s\n" exp;
  );
    print_res result0 result1
;;

test "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0"
