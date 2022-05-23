open Dlock
open Dlock.Printer
open Dlock.Types
open Format

module LambdaFlattenedSet = Set.Make(LambdaFlattened)

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
  printf "Deadlocked: \n0\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); printf "\n") deadlocked0;
  printf "1\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); printf "\n") deadlocked1;
  printf "Solved: \n0\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); printf "\n") solved0;
  printf "1\n";
  List.iter (fun l -> print_proc_simple fmt (lambdaFlattenedToProc l); printf "\n") solved1

let test (exp: string) = 
  let proc = CCS.parse exp in
  let result0 = conv_res (Deadlock_detector.main  proc) in
  let result1 = conv_res (Deadlock_detector2.main proc) in
  if compare_res result0 result1 then
    printf "PASSED: %s\n" exp
  else (
    printf "FAILED: %s\n" exp;
  );
    print_res result0 result1
;;

test "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0"
