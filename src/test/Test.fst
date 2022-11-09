module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f1fd820d2f0 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_CONST 3;
  LOAD_CONST 4;
  MAKE_FUNCTION 0;
  STORE_FAST 1;
  LOAD_CLOSURE 1;
  BUILD_TUPLE 1;
  LOAD_CONST 5;
  LOAD_CONST 6;
  MAKE_FUNCTION 8;
  STORE_DEREF 1;
  LOAD_CONST 7;
  LOAD_CONST 8;
  MAKE_FUNCTION 0;
  STORE_DEREF 0;
  LOAD_CLOSURE 0;
  BUILD_TUPLE 1;
  LOAD_CONST 9;
  LOAD_CONST 10;
  MAKE_FUNCTION 8;
  STORE_FAST 2;
  LOAD_FAST 0;
  CALL_FUNCTION 0;
  LOAD_FAST 1;
  LOAD_CONST 11;
  LOAD_CONST 12;
  LOAD_CONST 13;
  CALL_FUNCTION 3;
  LOAD_DEREF 1;
  LOAD_CONST 14;
  CALL_FUNCTION 1;
  LOAD_FAST 2;
  CALL_FUNCTION 0;
  BUILD_LIST 4;
  STORE_FAST 3;
  LOAD_FAST 3;
  RETURN_VALUE ;
]

let bc_0x7f1fd820d240 = CODE [
  LOAD_DEREF 0;
  CALL_FUNCTION 0;
  RETURN_VALUE ;
]

let consts_0x7f1fd820d240 = [
  PYTYP(createNone());
]

let varnames_0x7f1fd820d240 = [
]

let names_0x7f1fd820d240 = [
]

let cellvars_0x7f1fd820d240 = [
]

let freevars_0x7f1fd820d240 = [
  "test_function_to_be_called";
]

let co_0x7f1fd820d240 = {
  co_code = bc_0x7f1fd820d240;
  co_consts = consts_0x7f1fd820d240;
  co_varnames = varnames_0x7f1fd820d240;
  co_names = names_0x7f1fd820d240;
  co_cellvars = cellvars_0x7f1fd820d240;
  co_freevars = freevars_0x7f1fd820d240;
}

let bc_0x7f1fd820d190 = CODE [
  LOAD_CONST 1;
  RETURN_VALUE ;
]

let consts_0x7f1fd820d190 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
]

let varnames_0x7f1fd820d190 = [
]

let names_0x7f1fd820d190 = [
]

let cellvars_0x7f1fd820d190 = [
]

let freevars_0x7f1fd820d190 = [
]

let co_0x7f1fd820d190 = {
  co_code = bc_0x7f1fd820d190;
  co_consts = consts_0x7f1fd820d190;
  co_varnames = varnames_0x7f1fd820d190;
  co_names = names_0x7f1fd820d190;
  co_cellvars = cellvars_0x7f1fd820d190;
  co_freevars = freevars_0x7f1fd820d190;
}

let bc_0x7f1fd820d0e0 = CODE [
  LOAD_FAST 0;
  LOAD_CONST 1;
  COMPARE_OP 1;
  POP_JUMP_IF_FALSE 12;
  LOAD_CONST 1;
  RETURN_VALUE ;
  LOAD_FAST 0;
  LOAD_DEREF 0;
  LOAD_FAST 0;
  LOAD_CONST 2;
  BINARY_SUBTRACT ;
  CALL_FUNCTION 1;
  BINARY_ADD ;
  RETURN_VALUE ;
]

let consts_0x7f1fd820d0e0 = [
  PYTYP(createNone());
  PYTYP(createInt 0);
  PYTYP(createInt 1);
]

let varnames_0x7f1fd820d0e0 = [
  "x";
]

let names_0x7f1fd820d0e0 = [
]

let cellvars_0x7f1fd820d0e0 = [
]

let freevars_0x7f1fd820d0e0 = [
  "test_recursive";
]

let co_0x7f1fd820d0e0 = {
  co_code = bc_0x7f1fd820d0e0;
  co_consts = consts_0x7f1fd820d0e0;
  co_varnames = varnames_0x7f1fd820d0e0;
  co_names = names_0x7f1fd820d0e0;
  co_cellvars = cellvars_0x7f1fd820d0e0;
  co_freevars = freevars_0x7f1fd820d0e0;
}

let bc_0x7f1fd8200f50 = CODE [
  LOAD_FAST 0;
  LOAD_FAST 1;
  LOAD_FAST 2;
  BUILD_TUPLE 3;
  RETURN_VALUE ;
]

let consts_0x7f1fd8200f50 = [
  PYTYP(createNone());
]

let varnames_0x7f1fd8200f50 = [
  "x";
  "y";
  "z";
]

let names_0x7f1fd8200f50 = [
]

let cellvars_0x7f1fd8200f50 = [
]

let freevars_0x7f1fd8200f50 = [
]

let co_0x7f1fd8200f50 = {
  co_code = bc_0x7f1fd8200f50;
  co_consts = consts_0x7f1fd8200f50;
  co_varnames = varnames_0x7f1fd8200f50;
  co_names = names_0x7f1fd8200f50;
  co_cellvars = cellvars_0x7f1fd8200f50;
  co_freevars = freevars_0x7f1fd8200f50;
}

let bc_0x7f1fd8200df0 = CODE [
  LOAD_CONST 1;
  RETURN_VALUE ;
]

let consts_0x7f1fd8200df0 = [
  PYTYP(createNone());
  PYTYP(createInt 1);
]

let varnames_0x7f1fd8200df0 = [
]

let names_0x7f1fd8200df0 = [
]

let cellvars_0x7f1fd8200df0 = [
]

let freevars_0x7f1fd8200df0 = [
]

let co_0x7f1fd8200df0 = {
  co_code = bc_0x7f1fd8200df0;
  co_consts = consts_0x7f1fd8200df0;
  co_varnames = varnames_0x7f1fd8200df0;
  co_names = names_0x7f1fd8200df0;
  co_cellvars = cellvars_0x7f1fd8200df0;
  co_freevars = freevars_0x7f1fd8200df0;
}

let consts_0x7f1fd820d2f0 = [
  PYTYP(createString "Tests for calling/defining functions
    ");
  CODEOBJECT(co_0x7f1fd8200df0);
  PYTYP(createString "top_level.<locals>.test_function_def_with_no_params");
  CODEOBJECT(co_0x7f1fd8200f50);
  PYTYP(createString "top_level.<locals>.test_function_def_with_params");
  CODEOBJECT(co_0x7f1fd820d0e0);
  PYTYP(createString "top_level.<locals>.test_recursive");
  CODEOBJECT(co_0x7f1fd820d190);
  PYTYP(createString "top_level.<locals>.test_function_to_be_called");
  CODEOBJECT(co_0x7f1fd820d240);
  PYTYP(createString "top_level.<locals>.test_closure");
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createInt 3);
  PYTYP(createInt 5);
]

let varnames_0x7f1fd820d2f0 = [
  "test_function_def_with_no_params";
  "test_function_def_with_params";
  "test_closure";
  "result";
]

let names_0x7f1fd820d2f0 = [
]

let cellvars_0x7f1fd820d2f0 = [
  "test_function_to_be_called";
  "test_recursive";
]

let freevars_0x7f1fd820d2f0 = [
]

let co_0x7f1fd820d2f0 = {
  co_code = bc_0x7f1fd820d2f0;
  co_consts = consts_0x7f1fd820d2f0;
  co_varnames = varnames_0x7f1fd820d2f0;
  co_names = names_0x7f1fd820d2f0;
  co_cellvars = cellvars_0x7f1fd820d2f0;
  co_freevars = freevars_0x7f1fd820d2f0;
}

let virt_m, res = runCode_returnVM co_0x7f1fd820d2f0
let print_program_state = IO.print_string (print_program_state virt_m res)