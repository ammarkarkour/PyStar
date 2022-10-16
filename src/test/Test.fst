module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f2c2b61cdf0 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_CONST 3;
  LOAD_CONST 4;
  MAKE_FUNCTION 0;
  STORE_FAST 1;
  LOAD_CONST 5;
  LOAD_CONST 6;
  MAKE_FUNCTION 0;
  STORE_FAST 2;
  LOAD_FAST 0;
  CALL_FUNCTION 0;
  LOAD_FAST 1;
  CALL_FUNCTION 0;
  LOAD_FAST 2;
  CALL_FUNCTION 0;
  BUILD_LIST 3;
  STORE_FAST 3;
  LOAD_FAST 3;
  RETURN_VALUE ;
]

let bc_0x7f2c2b61cd40 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_CONST 1;
  STORE_FAST 2;
  LOAD_CONST 2;
  STORE_FAST 3;
  LOAD_CONST 2;
  STORE_FAST 4;
  LOAD_CONST 1;
  STORE_FAST 5;
  BUILD_LIST 0;
  UNARY_NOT ;
  STORE_FAST 6;
  LOAD_CONST 3;
  BUILD_LIST 1;
  UNARY_NOT ;
  STORE_FAST 7;
  LOAD_CONST 1;
  STORE_FAST 8;
  LOAD_CONST 2;
  STORE_FAST 9;
  BUILD_MAP 0;
  UNARY_NOT ;
  STORE_FAST 10;
  LOAD_CONST 3;
  LOAD_CONST 4;
  BUILD_MAP 1;
  UNARY_NOT ;
  STORE_FAST 11;
  LOAD_CONST 1;
  STORE_FAST 12;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  LOAD_FAST 3;
  LOAD_FAST 4;
  LOAD_FAST 5;
  LOAD_FAST 6;
  LOAD_FAST 7;
  LOAD_FAST 8;
  LOAD_FAST 9;
  LOAD_FAST 10;
  LOAD_FAST 11;
  LOAD_FAST 12;
  BUILD_TUPLE 13;
  RETURN_VALUE ;
]

let consts_0x7f2c2b61cd40 = [
  PYTYP(createNone());
  PYTYP(createBool true);
  PYTYP(createBool false);
  PYTYP(createInt 1);
  PYTYP(createInt 2);
]

let varnames_0x7f2c2b61cd40 = [
  "x11";
  "x12";
  "x21";
  "x22";
  "x31";
  "x32";
  "x41";
  "x42";
  "x51";
  "x52";
  "x61";
  "x62";
  "x7";
]

let names_0x7f2c2b61cd40 = [
]

let co_0x7f2c2b61cd40 = {
  co_code = bc_0x7f2c2b61cd40;
  co_consts = consts_0x7f2c2b61cd40;
  co_varnames = varnames_0x7f2c2b61cd40;
  co_names = names_0x7f2c2b61cd40
}

let bc_0x7f2c2b61cc90 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  LOAD_CONST 2;
  INPLACE_SUBTRACT ;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7f2c2b61cc90 = [
  PYTYP(createNone());
  PYTYP(createInt 2);
  PYTYP(createInt 1);
]

let varnames_0x7f2c2b61cc90 = [
  "x";
]

let names_0x7f2c2b61cc90 = [
]

let co_0x7f2c2b61cc90 = {
  co_code = bc_0x7f2c2b61cc90;
  co_consts = consts_0x7f2c2b61cc90;
  co_varnames = varnames_0x7f2c2b61cc90;
  co_names = names_0x7f2c2b61cc90
}

let bc_0x7f2c2b61cbe0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  BUILD_LIST 1;
  STORE_FAST 1;
  LOAD_CONST 2;
  STORE_FAST 2;
  LOAD_CONST 3;
  STORE_FAST 3;
  LOAD_FAST 0;
  LOAD_CONST 2;
  INPLACE_ADD ;
  STORE_FAST 0;
  LOAD_FAST 1;
  LOAD_CONST 4;
  BUILD_LIST 1;
  INPLACE_ADD ;
  STORE_FAST 1;
  LOAD_FAST 2;
  LOAD_CONST 4;
  INPLACE_ADD ;
  STORE_FAST 2;
  LOAD_FAST 3;
  LOAD_CONST 5;
  INPLACE_ADD ;
  STORE_FAST 3;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  LOAD_FAST 3;
  BUILD_TUPLE 4;
  RETURN_VALUE ;
]

let consts_0x7f2c2b61cbe0 = [
  PYTYP(createNone());
  PYTYP(createInt 0);
  PYTYP(createInt 1);
  PYTYP(createString "");
  PYTYP(createInt 2);
  PYTYP(createString "1");
]

let varnames_0x7f2c2b61cbe0 = [
  "x1";
  "x2";
  "x3";
  "x4";
]

let names_0x7f2c2b61cbe0 = [
]

let co_0x7f2c2b61cbe0 = {
  co_code = bc_0x7f2c2b61cbe0;
  co_consts = consts_0x7f2c2b61cbe0;
  co_varnames = varnames_0x7f2c2b61cbe0;
  co_names = names_0x7f2c2b61cbe0
}

let consts_0x7f2c2b61cdf0 = [
  PYTYP(createNone());
  CODEOBJECT(co_0x7f2c2b61cbe0);
  PYTYP(createString "top_level.<locals>.test_UNARY_POSITIVE");
  CODEOBJECT(co_0x7f2c2b61cc90);
  PYTYP(createString "top_level.<locals>.test_UNARY_NEGATIVE");
  CODEOBJECT(co_0x7f2c2b61cd40);
  PYTYP(createString "top_level.<locals>.test_UNARY_NOT");
]

let varnames_0x7f2c2b61cdf0 = [
  "test_UNARY_POSITIVE";
  "test_UNARY_NEGATIVE";
  "test_UNARY_NOT";
  "result";
]

let names_0x7f2c2b61cdf0 = [
]

let co_0x7f2c2b61cdf0 = {
  co_code = bc_0x7f2c2b61cdf0;
  co_consts = consts_0x7f2c2b61cdf0;
  co_varnames = varnames_0x7f2c2b61cdf0;
  co_names = names_0x7f2c2b61cdf0
}

let virt_m, res = runCode_returnVM co_0x7f2c2b61cdf0
let print_program_state = IO.print_string (print_program_state virt_m res)