module Test.fst

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f6859f9b240 = CODE [
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
  LOAD_CONST 7;
  LOAD_CONST 8;
  MAKE_FUNCTION 0;
  STORE_FAST 3;
  LOAD_CONST 9;
  LOAD_CONST 10;
  MAKE_FUNCTION 0;
  STORE_FAST 4;
  LOAD_CONST 11;
  LOAD_CONST 12;
  MAKE_FUNCTION 0;
  STORE_FAST 5;
  LOAD_CONST 13;
  LOAD_CONST 14;
  MAKE_FUNCTION 0;
  STORE_FAST 6;
  LOAD_FAST 0;
  CALL_FUNCTION 0;
  LOAD_FAST 1;
  CALL_FUNCTION 0;
  LOAD_FAST 2;
  CALL_FUNCTION 0;
  LOAD_FAST 3;
  CALL_FUNCTION 0;
  LOAD_FAST 4;
  CALL_FUNCTION 0;
  LOAD_FAST 5;
  CALL_FUNCTION 0;
  LOAD_FAST 6;
  CALL_FUNCTION 0;
  BUILD_LIST 7;
  STORE_FAST 7;
  LOAD_FAST 7;
  RETURN_VALUE ;
]

let bc_0x7f6859f9b190 = CODE [
  BUILD_LIST 0;
  STORE_FAST 0;
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
  STORE_FAST 1;
  LOAD_FAST 0;
  LOAD_CONST 0;
  LOAD_CONST 0;
  BUILD_SLICE 2;
  BINARY_SUBSCR ;
  STORE_FAST 2;
  LOAD_FAST 2;
  RETURN_VALUE ;
]

let consts_0x7f6859f9b190 = [
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
]

let varnames_0x7f6859f9b190 = [
  "l1";
  "l2";
  "x";
]

let names_0x7f6859f9b190 = [
]

let co_0x7f6859f9b190 = {
  co_code = bc_0x7f6859f9b190;
  co_consts = consts_0x7f6859f9b190;
  co_varnames = varnames_0x7f6859f9b190;
  co_names = names_0x7f6859f9b190
}

let bc_0x7f6859f9b0e0 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  LOAD_CONST 3;
  BUILD_LIST 3;
  LOAD_CONST 1;
  BINARY_SUBSCR ;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7f6859f9b0e0 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createInt 3);
]

let varnames_0x7f6859f9b0e0 = [
  "x1";
]

let names_0x7f6859f9b0e0 = [
]

let co_0x7f6859f9b0e0 = {
  co_code = bc_0x7f6859f9b0e0;
  co_consts = consts_0x7f6859f9b0e0;
  co_varnames = varnames_0x7f6859f9b0e0;
  co_names = names_0x7f6859f9b0e0
}

let bc_0x7f6859f9b030 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_FAST 0;
  LOAD_FAST 1;
  BUILD_TUPLE 2;
  RETURN_VALUE ;
]

let consts_0x7f6859f9b030 = [
  PYTYP(createNone());
  PYTYP(createInt 8);
  PYTYP(createInt (-1));
]

let varnames_0x7f6859f9b030 = [
  "x1";
  "x2";
]

let names_0x7f6859f9b030 = [
]

let co_0x7f6859f9b030 = {
  co_code = bc_0x7f6859f9b030;
  co_consts = consts_0x7f6859f9b030;
  co_varnames = varnames_0x7f6859f9b030;
  co_names = names_0x7f6859f9b030
}

let bc_0x7f6859f94f50 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 1;
  BUILD_LIST 1;
  LOAD_CONST 2;
  BUILD_LIST 1;
  BINARY_ADD ;
  STORE_FAST 1;
  LOAD_CONST 3;
  STORE_FAST 2;
  LOAD_CONST 4;
  STORE_FAST 3;
  LOAD_CONST 5;
  STORE_FAST 4;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  LOAD_FAST 3;
  LOAD_FAST 4;
  BUILD_TUPLE 5;
  RETURN_VALUE ;
]

let consts_0x7f6859f94f50 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createTuple([createInt 1; createInt 2; createInt 3; createInt 4; ]));
  PYTYP(createString "12");
  PYTYP(createInt 4);
]

let varnames_0x7f6859f94f50 = [
  "x1";
  "x2";
  "x3";
  "x4";
  "x5";
]

let names_0x7f6859f94f50 = [
]

let co_0x7f6859f94f50 = {
  co_code = bc_0x7f6859f94f50;
  co_consts = consts_0x7f6859f94f50;
  co_varnames = varnames_0x7f6859f94f50;
  co_names = names_0x7f6859f94f50
}

let bc_0x7f6859f94ea0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_CONST 3;
  STORE_FAST 2;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  BUILD_TUPLE 3;
  RETURN_VALUE ;
]

let consts_0x7f6859f94ea0 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createInt 0);
]

let varnames_0x7f6859f94ea0 = [
  "x1";
  "x2";
  "x3";
]

let names_0x7f6859f94ea0 = [
]

let co_0x7f6859f94ea0 = {
  co_code = bc_0x7f6859f94ea0;
  co_consts = consts_0x7f6859f94ea0;
  co_varnames = varnames_0x7f6859f94ea0;
  co_names = names_0x7f6859f94ea0
}

let bc_0x7f6859f94df0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_CONST 2;
  STORE_FAST 2;
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  BUILD_TUPLE 3;
  RETURN_VALUE ;
]

let consts_0x7f6859f94df0 = [
  PYTYP(createNone());
  PYTYP(createInt 5);
  PYTYP(createInt 0);
]

let varnames_0x7f6859f94df0 = [
  "x1";
  "x2";
  "x3";
]

let names_0x7f6859f94df0 = [
]

let co_0x7f6859f94df0 = {
  co_code = bc_0x7f6859f94df0;
  co_consts = consts_0x7f6859f94df0;
  co_varnames = varnames_0x7f6859f94df0;
  co_names = names_0x7f6859f94df0
}

let bc_0x7f6859f94d40 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_CONST 2;
  STORE_FAST 1;
  LOAD_CONST 3;
  STORE_FAST 2;
  LOAD_CONST 4;
  STORE_FAST 3;
  LOAD_CONST 4;
  STORE_FAST 4;
  LOAD_CONST 5;
  BUILD_LIST 1;
  LOAD_CONST 3;
  BINARY_MULTIPLY ;
  STORE_FAST 5;
  LOAD_CONST 3;
  LOAD_CONST 5;
  BUILD_LIST 1;
  BINARY_MULTIPLY ;
  STORE_FAST 6;
  LOAD_CONST 3;
  STORE_FAST 7;
  LOAD_CONST 3;
  STORE_FAST 8;
  LOAD_CONST 6;
  STORE_FAST 9;
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
  BUILD_TUPLE 10;
  RETURN_VALUE ;
]

let consts_0x7f6859f94d40 = [
  PYTYP(createNone());
  PYTYP(createInt 6);
  PYTYP(createInt 0);
  PYTYP(createInt 2);
  PYTYP(createString "11");
  PYTYP(createInt 1);
  PYTYP(createString "same");
]

let varnames_0x7f6859f94d40 = [
  "x1";
  "x2";
  "x3";
  "x4";
  "x5";
  "x6";
  "x7";
  "x8";
  "x9";
  "x10";
]

let names_0x7f6859f94d40 = [
]

let co_0x7f6859f94d40 = {
  co_code = bc_0x7f6859f94d40;
  co_consts = consts_0x7f6859f94d40;
  co_varnames = varnames_0x7f6859f94d40;
  co_names = names_0x7f6859f94d40
}

let consts_0x7f6859f9b240 = [
  PYTYP(createString "Test cases for Binary operations
    ");
  CODEOBJECT(co_0x7f6859f94d40);
  PYTYP(createString "top_level.<locals>.test_BINARY_MULTIPLY");
  CODEOBJECT(co_0x7f6859f94df0);
  PYTYP(createString "top_level.<locals>.test_BINARY_FLOOR_DIVIDE");
  CODEOBJECT(co_0x7f6859f94ea0);
  PYTYP(createString "top_level.<locals>.test_BINARY_MODULO");
  CODEOBJECT(co_0x7f6859f94f50);
  PYTYP(createString "top_level.<locals>.test_BINARY_ADD");
  CODEOBJECT(co_0x7f6859f9b030);
  PYTYP(createString "top_level.<locals>.test_BINARY_SUBTRACT");
  CODEOBJECT(co_0x7f6859f9b0e0);
  PYTYP(createString "top_level.<locals>.test_BINARY_SUBSCR");
  CODEOBJECT(co_0x7f6859f9b190);
  PYTYP(createString "top_level.<locals>.test_BINARY_SUBSCR_SLICES");
]

let varnames_0x7f6859f9b240 = [
  "test_BINARY_MULTIPLY";
  "test_BINARY_FLOOR_DIVIDE";
  "test_BINARY_MODULO";
  "test_BINARY_ADD";
  "test_BINARY_SUBTRACT";
  "test_BINARY_SUBSCR";
  "test_BINARY_SUBSCR_SLICES";
  "result";
]

let names_0x7f6859f9b240 = [
]

let co_0x7f6859f9b240 = {
  co_code = bc_0x7f6859f9b240;
  co_consts = consts_0x7f6859f9b240;
  co_varnames = varnames_0x7f6859f9b240;
  co_names = names_0x7f6859f9b240
}

let virt_m, res = runCode_returnVM co_0x7f6859f9b240
let print_program_state = IO.print_string (print_program_state virt_m res)