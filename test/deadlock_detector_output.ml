open Dlock
open Dlock.Auxfunctions
open Dlock.Types
let test expr: string = 
  Cmd.simplified := true;
  [1; 2] |>
  List.iter (
    fun ds_num ->
      Cmd.ds := ds_num;
      ignore (Deadlock_detector.main Format.std_formatter (CCS.parse expr))
  )
  ; ""
;;

test "a!.0 || a?.0"