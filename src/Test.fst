module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f8f4540c450 = CODE [
  LOAD_CONST 0;
  STORE_NAME 0;
  LOAD_CONST 1;
  RETURN_VALUE ;
]

let consts_0x7f8f4540c450 = [
  PYTYP(createInt 1);
  PYTYP(createNone());
]

let varnames_0x7f8f4540c450 = [
]

let names_0x7f8f4540c450 = [
  "x";
]

let co_0x7f8f4540c450 = {
  co_code = bc_0x7f8f4540c450;
  co_consts = consts_0x7f8f4540c450;
  co_varnames = varnames_0x7f8f4540c450;
  co_names = names_0x7f8f4540c450
}

let res, virt_m = runCode_returnVM co_0x7f8f4540c450
let print_program_state = IO.print_string (print_program_state res virt_m)