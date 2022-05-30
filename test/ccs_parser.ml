open Dlock

type test_result = Success
  | Failure of Types.proc * Types.proc

let (&&) (a: test_result) (b: test_result): test_result =
  match a, b with
    | Success, Success -> Success
    | Success, (Failure(_, _) as f) -> f
    | (Failure(_, _) as f), Success -> f
    | (Failure(_, _) as f1), (Failure(_, _)) -> f1 (* TODO *)

let (==) (a: Types.action) (b: Types.action) : bool =
  match a, b with
    | AIn(a1), AIn(b1) -> a1 == b1
    | AOut(a1), AOut(b1) -> a1 == b1
    | _ -> false

let rec proc_equals (a: Types.proc) (b: Types.proc) : test_result =
  match a, b with
    | PNil, PNil -> Success
    | POrI(a1, a2), POrI(b1, b2) -> (proc_equals a1 b1) && (proc_equals a2 b2)
    | PPref(a1, a2), PPref(b1, b2) -> (match (a1 == b1), (proc_equals a2 b2) with
                                        | true, Success -> Success
                                        | _, (Failure(_, _) as f) -> f
                                        | false, _ -> Failure(a, b))
    | PPar(a1, a2), PPar(b1, b2) -> (proc_equals a1 b1) && (proc_equals a2 b2)
    | _ -> Failure(a, b)

let fmt = Format.std_formatter

let parse_and_test (s: string) (p: Types.proc) =
  let parsed = CCS.parse(s) in 
    match proc_equals parsed p with
      | Failure(p1, p2) -> (
        Format.fprintf fmt "FAILED:\n";
        Format.fprintf fmt "GOT          :"; (Printer.print_proc_simple fmt p1);     Format.fprintf fmt "\n"; 
        Format.fprintf fmt "EXPECTED     :"; (Printer.print_proc_simple fmt p2);     Format.fprintf fmt "\n"; 
        Format.fprintf fmt "GOT FULL     :"; (Printer.print_proc_simple fmt parsed); Format.fprintf fmt "\n"; 
        Format.fprintf fmt "EXPECTED FULL:"; (Printer.print_proc_simple fmt p);      Format.fprintf fmt "\n"; 
        exit 1)
      | Success -> Format.fprintf fmt "SUCCESS:"; (Printer.print_proc_simple fmt p); Format.fprintf fmt "\n";
;;

Format.fprintf fmt "CCS_PARSER:\n";
parse_and_test "(a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0" ( POrI(PPar(PPref(AOut('a'), PPref(AIn('a'), PNil)), PPref(AIn('b'), PPref(AOut('b'), PPref(AIn('c'), PPref(AOut('c'), PNil))))), PPref(AOut('c'), PPref(AIn('c'), PNil))) );
parse_and_test "a!.0 || (b!.b?.a?.0 + a?.0)" (PPar(PPref(AOut('a'), PNil), POrI(PPref(AOut('b'), PPref(AIn('b'), PPref(AIn('a'), PNil))), PPref(AIn('a'), PNil))));
parse_and_test "a?.(c?.0 + d?.0) || a!.e!.0" (PPar(PPref(AIn('a'), POrI(PPref(AIn('c'), PNil), PPref(AIn('d'), PNil))), PPref(AOut('a'), PPref(AOut('e'), PNil))) );
parse_and_test "a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0" ( PPar(PPref(AOut('a'), PPar(PPref(AOut('b'), PPref(AOut('c'), PNil)), PPref(AIn('b'), PPref(AIn('c'), PPref(AIn('d'), PNil))))), PPref(AIn('a'), PPref(AOut('d'), PNil))) );