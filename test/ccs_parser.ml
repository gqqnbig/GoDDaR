open Dlock
open Dlock.Types

type test_result = Success
  | Failure of Lambda.t * Lambda.t

let (&&) (a: test_result) (b: test_result): test_result =
  match a, b with
    | Success, Success -> Success
    | Success, (Failure(_, _) as f) -> f
    | (Failure(_, _) as f), Success -> f
    | (Failure(_, _) as f1), (Failure(_, _)) -> f1 (* TODO *)

let (==) (a: Types.action) (b: Types.action) : bool =
  match a, b with
    | AIn(a1), AIn(b1) -> a1 = b1
    | AOut(a1), AOut(b1) -> a1 = b1
    | _ -> false

let rec proc_equals (a: Lambda.t) (b: Lambda.t) : test_result =
  match a, b with
    | LNil, LNil -> Success
    | LOrI(a1, a2), LOrI(b1, b2) -> (proc_equals a1 b1) && (proc_equals a2 b2)
    | LList(a1, a2), LList(b1, b2) -> (match (a1 = b1), (proc_equals a2 b2) with
                                        | true, Success -> Success
                                        | _, (Failure(_, _) as f) -> f
                                        | false, _ -> Failure(a, b))
    | LPar(a1, a2), LPar(b1, b2) -> (proc_equals a1 b1) && (proc_equals a2 b2)
    | _ -> Failure(a, b)

let fmt = Format.std_formatter

let parse_and_test (s: string) (p: Lambda.t) =
  let parsed = Types.lambdaTaggedToLambda (CCS.parse(s)) in 
    match proc_equals parsed p with
      | Failure(p1, p2) -> (
        Format.fprintf fmt "FAILED:\n";
        Format.fprintf fmt "GOT          :"; (Lambda.print_lambda_simple fmt p1);     Format.fprintf fmt "\n"; 
        Format.fprintf fmt "EXPECTED     :"; (Lambda.print_lambda_simple fmt p2);     Format.fprintf fmt "\n"; 
        Format.fprintf fmt "GOT FULL     :"; (Lambda.print_lambda_simple fmt parsed); Format.fprintf fmt "\n"; 
        Format.fprintf fmt "EXPECTED FULL:"; (Lambda.print_lambda_simple fmt p);      Format.fprintf fmt "\n"; 
        exit 1)
      | Success -> Format.fprintf fmt "SUCCESS:"; (Lambda.print_lambda_simple fmt p); Format.fprintf fmt "\n";
;;

Format.fprintf fmt "CCS_PARSER:\n";
parse_and_test "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0" ( LOrI(LPar(LList(EEta(AOut("a")), LList(EEta(AIn("a")), LNil)), LList(EEta(AIn("b")), LList(EEta(AOut("b")), LList(EEta(AIn("c")), LList(EEta(AOut("c")), LNil))))), LList(EEta(AOut("c")), LList(EEta(AIn("c")), LNil))) );
parse_and_test "a!.0 || (b!.b?.a?.0 + a?.0)" (LPar(LList(EEta(AOut("a")), LNil), LOrI(LList(EEta(AOut("b")), LList(EEta(AIn("b")), LList(EEta(AIn("a")), LNil))), LList(EEta(AIn("a")), LNil))));
parse_and_test "a?.(c?.0 + d?.0) || a!.e!.0" (LPar(LList(EEta(AIn("a")), LOrI(LList(EEta(AIn("c")), LNil), LList(EEta(AIn("d")), LNil))), LList(EEta(AOut("a")), LList(EEta(AOut("e")), LNil))) );
parse_and_test "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0" ( LPar(LList(EEta(AOut("a")), LPar(LList(EEta(AOut("b")), LList(EEta(AOut("c")), LNil)), LList(EEta(AIn("b")), LList(EEta(AIn("c")), LList(EEta(AIn("d")), LNil))))), LList(EEta(AIn("a")), LList(EEta(AOut("d")), LNil))) );