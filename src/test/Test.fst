module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7fec7faa6a80 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_FAST 0;
  CALL_FUNCTION 0;
  BUILD_LIST 1;
  STORE_FAST 1;
  LOAD_FAST 1;
  RETURN_VALUE ;
]

let bc_0x7fec7faa69d0 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  LOAD_CONST 3;
  LOAD_CONST 4;
  LOAD_CONST 5;
  LOAD_CONST 6;
  LOAD_CONST 7;
  LOAD_CONST 8;
  LOAD_CONST 9;
  BUILD_LIST 9;
  STORE_FAST 0;
  LOAD_FAST 0;
  LOAD_CONST 0;
  LOAD_CONST 0;
  BUILD_SLICE 2;
  BINARY_SUBSCR ;
  STORE_FAST 1;
  LOAD_FAST 0;
  LOAD_CONST 0;
  LOAD_CONST 0;
  LOAD_CONST 10;
  BUILD_SLICE 3;
  BINARY_SUBSCR ;
  STORE_FAST 2;
  LOAD_FAST 0;
  LOAD_CONST 0;
  LOAD_CONST 0;
  LOAD_CONST 2;
  BUILD_SLICE 3;
  BINARY_SUBSCR ;
  STORE_FAST 3;
  LOAD_FAST 0;
  LOAD_CONST 0;
  LOAD_CONST 0;
  LOAD_CONST 11;
  BUILD_SLICE 3;
  BINARY_SUBSCR ;
  STORE_FAST 4;
  LOAD_FAST 0;
  LOAD_CONST 12;
  LOAD_CONST 13;
  BUILD_SLICE 2;
  BINARY_SUBSCR ;
  STORE_FAST 5;
  LOAD_FAST 0;
  LOAD_CONST 14;
  LOAD_CONST 0;
  LOAD_CONST 10;
  BUILD_SLICE 3;
  BINARY_SUBSCR ;
  STORE_FAST 6;
  LOAD_FAST 0;
  LOAD_CONST 5;
  LOAD_CONST 1;
  BUILD_SLICE 2;
  BINARY_SUBSCR ;
  STORE_FAST 7;
  LOAD_FAST 0;
  LOAD_CONST 1;
  LOAD_CONST 5;
  LOAD_CONST 10;
  BUILD_SLICE 3;
  BINARY_SUBSCR ;
  STORE_FAST 8;
  LOAD_FAST 1;
  LOAD_FAST 2;
  LOAD_FAST 3;
  LOAD_FAST 4;
  LOAD_FAST 5;
  LOAD_FAST 6;
  LOAD_FAST 7;
  LOAD_FAST 8;
  BUILD_TUPLE 8;
  RETURN_VALUE ;
]

let consts_0x7fec7faa69d0 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createInt 3);
  PYTYP(createInt 4);
  PYTYP(createInt 5);
  PYTYP(createInt 6);
  PYTYP(createInt 7);
  PYTYP(createInt 8);
  PYTYP(createInt 9);
  PYTYP(createInt (-1));
  PYTYP(createInt (-2));
  PYTYP(createInt 0);
  PYTYP(createInt 15);
  PYTYP(createInt (-10));
]

let varnames_0x7fec7faa69d0 = [
  "l2";
  "x1";
  "x2";
  "x3";
  "x4";
  "x5";
  "x6";
  "x7";
  "x8";
]

let names_0x7fec7faa69d0 = [
]

let co_0x7fec7faa69d0 = {
  co_code = bc_0x7fec7faa69d0;
  co_consts = consts_0x7fec7faa69d0;
  co_varnames = varnames_0x7fec7faa69d0;
  co_names = names_0x7fec7faa69d0
}

let consts_0x7fec7faa6a80 = [
  PYTYP(createString "Test cases for Binary operations
    ");
  CODEOBJECT(co_0x7fec7faa69d0);
  PYTYP(createString "top_level.<locals>.test_BINARY_SUBSCR_SLICES");
]

let varnames_0x7fec7faa6a80 = [
  "test_BINARY_SUBSCR_SLICES";
  "result";
]

let names_0x7fec7faa6a80 = [
]

let co_0x7fec7faa6a80 = {
  co_code = bc_0x7fec7faa6a80;
  co_consts = consts_0x7fec7faa6a80;
  co_varnames = varnames_0x7fec7faa6a80;
  co_names = names_0x7fec7faa6a80
}

let virt_m, res = runCode_returnVM co_0x7fec7faa6a80
let print_program_state = IO.print_string (print_program_state virt_m res)