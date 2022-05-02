open Dlock

let fmt = Format.std_formatter
;;

let expression = "a!.b?.0 + c?.d!.0 || a!.b?.0" in
    Format.fprintf fmt "%s\n" expression;
    let proc = (CCS.parse expression) in
        Printer.print_proc_simple fmt proc;
        Printer.print_proc fmt proc;
        Format.fprintf fmt "\n"