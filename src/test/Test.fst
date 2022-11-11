module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f6b9ce4b9d0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_CONST 3;
  STORE_FAST 2;
  LOAD_FAST 0;
  LOAD_FAST 1;
  BINARY_MULTIPLY ;
  LOAD_CONST 3;
  BINARY_MULTIPLY ;
  STORE_FAST 3;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  LOAD_FAST 3;
  BUILD_TUPLE 4;
  RETURN_VALUE ;
]

let consts_0x7f6b9ce4b9d0 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createInt 3);
]

let varnames_0x7f6b9ce4b9d0 = [
  "var_1";
  "var_2";
  "var_3";
  "var_4";
]

let names_0x7f6b9ce4b9d0 = [
]

let cellvars_0x7f6b9ce4b9d0 = [
]

let freevars_0x7f6b9ce4b9d0 = [
]

let co_0x7f6b9ce4b9d0 = {
  co_code = bc_0x7f6b9ce4b9d0;
  co_consts = consts_0x7f6b9ce4b9d0;
  co_varnames = varnames_0x7f6b9ce4b9d0;
  co_names = names_0x7f6b9ce4b9d0;
  co_cellvars = cellvars_0x7f6b9ce4b9d0;
  co_freevars = freevars_0x7f6b9ce4b9d0;
}

let virt_m, res = runCode_returnVM co_0x7f6b9ce4b9d0
let print_program_state = IO.print_string (print_program_state virt_m res)