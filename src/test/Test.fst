module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7fefbe736660 = CODE [
  LOAD_CONST 0;
  LOAD_CONST 1;
  MAKE_FUNCTION 0;
  STORE_NAME 0;
  LOAD_CONST 2;
  LOAD_CONST 3;
  MAKE_FUNCTION 0;
  STORE_NAME 1;
  LOAD_CONST 4;
  RETURN_VALUE ;
]

let bc_0x7fefbe7365b0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7fefbe7365b0 = [
  PYTYP(createNone());
  PYTYP(createInt 7);
]

let varnames_0x7fefbe7365b0 = [
  "y";
]

let names_0x7fefbe7365b0 = [
]

let co_0x7fefbe7365b0 = {
  co_code = bc_0x7fefbe7365b0;
  co_consts = consts_0x7fefbe7365b0;
  co_varnames = varnames_0x7fefbe7365b0;
  co_names = names_0x7fefbe7365b0
}

let bc_0x7fefbe736450 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7fefbe736450 = [
  PYTYP(createNone());
  PYTYP(createInt 5);
]

let varnames_0x7fefbe736450 = [
  "x";
]

let names_0x7fefbe736450 = [
]

let co_0x7fefbe736450 = {
  co_code = bc_0x7fefbe736450;
  co_consts = consts_0x7fefbe736450;
  co_varnames = varnames_0x7fefbe736450;
  co_names = names_0x7fefbe736450
}

let consts_0x7fefbe736660 = [
  CODEOBJECT(co_0x7fefbe736450);
  PYTYP(createString "first_function");
  CODEOBJECT(co_0x7fefbe7365b0);
  PYTYP(createString "second_function");
  PYTYP(createNone());
]

let varnames_0x7fefbe736660 = [
]

let names_0x7fefbe736660 = [
  "first_function";
  "second_function";
]

let co_0x7fefbe736660 = {
  co_code = bc_0x7fefbe736660;
  co_consts = consts_0x7fefbe736660;
  co_varnames = varnames_0x7fefbe736660;
  co_names = names_0x7fefbe736660
}

let res, virt_m = runCode_returnVM co_0x7fefbe736660
let print_program_state = IO.print_string (print_program_state res virt_m)