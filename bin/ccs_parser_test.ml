open Dlock

let fmt = Format.std_formatter

let print_action fmt a =
    match a with
    | Types.AIn(a) -> Format.fprintf fmt "AIn(%c)" a 
    | Types.AOut(a) -> Format.fprintf fmt "AOut(%c)" a 

let print_action_simple fmt a =
    match a with
    | Types.AIn(a) -> Format.fprintf fmt "%c?" a 
    | Types.AOut(a) -> Format.fprintf fmt "%c!" a

let rec do_print_proc fmt p =
    match p with
    | Types.PNil -> Format.fprintf fmt "PNil"
    | Types.POr(p1, p2) -> Format.fprintf fmt "POr(%a, %a)" do_print_proc p1 do_print_proc p2
    | Types.PPref(a, pp) -> Format.fprintf fmt "PPref(%a, %a)" print_action a do_print_proc pp
    | Types.PPar(p1, p2) -> Format.fprintf fmt "PPar(%a, %a)" do_print_proc p1 do_print_proc p2

let print_proc fmt p = 
    do_print_proc fmt p;
    Format.fprintf fmt "\n"

let rec do_print_proc_simple fmt p =
    match p with
    | Types.PNil -> Format.fprintf fmt "0"
    | Types.POr(p1, p2) -> Format.fprintf fmt "(%a + %a)" do_print_proc_simple p1 do_print_proc_simple p2
    | Types.PPref(a, pp) -> Format.fprintf fmt "%a.%a" print_action_simple a do_print_proc_simple pp
    | Types.PPar(p1, p2) -> Format.fprintf fmt "(%a || %a)" do_print_proc_simple p1 do_print_proc_simple p2

let print_proc_simple fmt p = 
    do_print_proc_simple fmt p;
    Format.fprintf fmt "\n"

;;

let expression = "a!.b?.0 + c?.d!.0 || a!.b?.0" in
    Format.fprintf fmt "%s\n" expression;
    let proc = (CCS.parse expression) in
        print_proc_simple fmt proc;
        print_proc fmt proc;
        Format.fprintf fmt "\n"