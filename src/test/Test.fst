module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f92191c0c90 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_CONST 3;
  LOAD_CONST 4;
  MAKE_FUNCTION 0;
  STORE_FAST 1;
  LOAD_FAST 0;
  CALL_FUNCTION 0;
  LOAD_FAST 1;
  CALL_FUNCTION 0;
  BINARY_MULTIPLY ;
  RETURN_VALUE ;
]

let bc_0x7f92191c0be0 = CODE [
  LOAD_CONST 1;
  RETURN_VALUE ;
]

let consts_0x7f92191c0be0 = [
  PYTYP(createNone());
  PYTYP(createInt 3);
]

let varnames_0x7f92191c0be0 = [
]

let names_0x7f92191c0be0 = [
]

let co_0x7f92191c0be0 = {
  co_code = bc_0x7f92191c0be0;
  co_consts = consts_0x7f92191c0be0;
  co_varnames = varnames_0x7f92191c0be0;
  co_names = names_0x7f92191c0be0
}

let bc_0x7f92191c0a80 = CODE [
  LOAD_CONST 1;
  RETURN_VALUE ;
]

let consts_0x7f92191c0a80 = [
  PYTYP(createNone());
  PYTYP(createInt 7);
]

let varnames_0x7f92191c0a80 = [
]

let names_0x7f92191c0a80 = [
]

let co_0x7f92191c0a80 = {
  co_code = bc_0x7f92191c0a80;
  co_consts = consts_0x7f92191c0a80;
  co_varnames = varnames_0x7f92191c0a80;
  co_names = names_0x7f92191c0a80
}

let consts_0x7f92191c0c90 = [
  PYTYP(createNone());
  CODEOBJECT(co_0x7f92191c0a80);
  PYTYP(createString "top_level.<locals>.test");
  CODEOBJECT(co_0x7f92191c0be0);
  PYTYP(createString "top_level.<locals>.test2");
]

let varnames_0x7f92191c0c90 = [
  "test";
  "test2";
]

let names_0x7f92191c0c90 = [
]

let co_0x7f92191c0c90 = {
  co_code = bc_0x7f92191c0c90;
  co_consts = consts_0x7f92191c0c90;
  co_varnames = varnames_0x7f92191c0c90;
  co_names = names_0x7f92191c0c90
}

let virt_m, res = runCode_returnVM co_0x7f92191c0c90
let print_program_state = IO.print_string (print_program_state virt_m res)