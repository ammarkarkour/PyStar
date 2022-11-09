module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7fb3abf9d500 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_DEREF 0;
  LOAD_CONST 3;
  LOAD_CONST 4;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_CLOSURE 0;
  BUILD_TUPLE 1;
  LOAD_CONST 5;
  LOAD_CONST 6;
  MAKE_FUNCTION 8;
  STORE_FAST 1;
  LOAD_CLOSURE 0;
  BUILD_TUPLE 1;
  LOAD_CONST 7;
  LOAD_CONST 8;
  MAKE_FUNCTION 8;
  STORE_FAST 2;
  LOAD_CLOSURE 0;
  BUILD_TUPLE 1;
  LOAD_CONST 9;
  LOAD_CONST 10;
  MAKE_FUNCTION 8;
  STORE_FAST 3;
  LOAD_CLOSURE 0;
  BUILD_TUPLE 1;
  LOAD_CONST 11;
  LOAD_CONST 12;
  MAKE_FUNCTION 8;
  STORE_FAST 4;
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
  BUILD_LIST 5;
  STORE_FAST 5;
  LOAD_FAST 5;
  RETURN_VALUE ;
]

let bc_0x7fb3abf9d450 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  LOAD_CONST 3;
  LOAD_CONST 4;
  LOAD_CONST 5;
  LOAD_CONST 6;
  BUILD_CONST_KEY_MAP 5;
  STORE_FAST 0;
  LOAD_DEREF 0;
  LOAD_FAST 0;
  CALL_FUNCTION 1;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d450 = [
  PYTYP(createNone());
  PYTYP(createInt 2);
  PYTYP(createInt 4);
  PYTYP(createInt 6);
  PYTYP(createInt 8);
  PYTYP(createInt 10);
  PYTYP(createTuple([createInt 1; createInt 3; createInt 5; createInt 7; createInt 9; ]));
]

let varnames_0x7fb3abf9d450 = [
  "d";
]

let names_0x7fb3abf9d450 = [
]

let cellvars_0x7fb3abf9d450 = [
]

let freevars_0x7fb3abf9d450 = [
  "run_for_loop";
]

let co_0x7fb3abf9d450 = {
  co_code = bc_0x7fb3abf9d450;
  co_consts = consts_0x7fb3abf9d450;
  co_varnames = varnames_0x7fb3abf9d450;
  co_names = names_0x7fb3abf9d450;
  co_cellvars = cellvars_0x7fb3abf9d450;
  co_freevars = freevars_0x7fb3abf9d450;
}

let bc_0x7fb3abf9d3a0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_DEREF 0;
  LOAD_FAST 0;
  CALL_FUNCTION 1;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d3a0 = [
  PYTYP(createNone());
  PYTYP(createString "0123456789");
]

let varnames_0x7fb3abf9d3a0 = [
  "s";
]

let names_0x7fb3abf9d3a0 = [
]

let cellvars_0x7fb3abf9d3a0 = [
]

let freevars_0x7fb3abf9d3a0 = [
  "run_for_loop";
]

let co_0x7fb3abf9d3a0 = {
  co_code = bc_0x7fb3abf9d3a0;
  co_consts = consts_0x7fb3abf9d3a0;
  co_varnames = varnames_0x7fb3abf9d3a0;
  co_names = names_0x7fb3abf9d3a0;
  co_cellvars = cellvars_0x7fb3abf9d3a0;
  co_freevars = freevars_0x7fb3abf9d3a0;
}

let bc_0x7fb3abf9d2f0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_DEREF 0;
  LOAD_FAST 0;
  CALL_FUNCTION 1;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d2f0 = [
  PYTYP(createNone());
  PYTYP(createTuple([createInt 0; createInt 1; createInt 2; createInt 3; createInt 4; createInt 5; createInt 6; createInt 7; createInt 8; createInt 9; ]));
]

let varnames_0x7fb3abf9d2f0 = [
  "t";
]

let names_0x7fb3abf9d2f0 = [
]

let cellvars_0x7fb3abf9d2f0 = [
]

let freevars_0x7fb3abf9d2f0 = [
  "run_for_loop";
]

let co_0x7fb3abf9d2f0 = {
  co_code = bc_0x7fb3abf9d2f0;
  co_consts = consts_0x7fb3abf9d2f0;
  co_varnames = varnames_0x7fb3abf9d2f0;
  co_names = names_0x7fb3abf9d2f0;
  co_cellvars = cellvars_0x7fb3abf9d2f0;
  co_freevars = freevars_0x7fb3abf9d2f0;
}

let bc_0x7fb3abf9d240 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  LOAD_CONST 3;
  LOAD_CONST 4;
  LOAD_CONST 5;
  LOAD_CONST 6;
  LOAD_CONST 7;
  LOAD_CONST 8;
  LOAD_CONST 9;
  LOAD_CONST 10;
  BUILD_LIST 10;
  STORE_FAST 0;
  LOAD_DEREF 0;
  LOAD_FAST 0;
  CALL_FUNCTION 1;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d240 = [
  PYTYP(createNone());
  PYTYP(createInt 0);
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

let varnames_0x7fb3abf9d240 = [
  "l";
]

let names_0x7fb3abf9d240 = [
]

let cellvars_0x7fb3abf9d240 = [
]

let freevars_0x7fb3abf9d240 = [
  "run_for_loop";
]

let co_0x7fb3abf9d240 = {
  co_code = bc_0x7fb3abf9d240;
  co_consts = consts_0x7fb3abf9d240;
  co_varnames = varnames_0x7fb3abf9d240;
  co_names = names_0x7fb3abf9d240;
  co_cellvars = cellvars_0x7fb3abf9d240;
  co_freevars = freevars_0x7fb3abf9d240;
}

let bc_0x7fb3abf9d190 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  LOAD_CONST 2;
  COMPARE_OP 0;
  POP_JUMP_IF_FALSE 22;
  LOAD_FAST 0;
  LOAD_CONST 3;
  INPLACE_ADD ;
  STORE_FAST 0;
  JUMP_ABSOLUTE 4;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d190 = [
  PYTYP(createNone());
  PYTYP(createInt 0);
  PYTYP(createInt 10);
  PYTYP(createInt 1);
]

let varnames_0x7fb3abf9d190 = [
  "x";
]

let names_0x7fb3abf9d190 = [
]

let cellvars_0x7fb3abf9d190 = [
]

let freevars_0x7fb3abf9d190 = [
]

let co_0x7fb3abf9d190 = {
  co_code = bc_0x7fb3abf9d190;
  co_consts = consts_0x7fb3abf9d190;
  co_varnames = varnames_0x7fb3abf9d190;
  co_names = names_0x7fb3abf9d190;
  co_cellvars = cellvars_0x7fb3abf9d190;
  co_freevars = freevars_0x7fb3abf9d190;
}

let bc_0x7fb3abf9d0e0 = CODE [
  BUILD_LIST 0;
  STORE_FAST 1;
  LOAD_FAST 0;
  GET_ITER ;
  FOR_ITER 14;
  STORE_FAST 2;
  LOAD_FAST 1;
  LOAD_FAST 2;
  BUILD_LIST 1;
  INPLACE_ADD ;
  STORE_FAST 1;
  JUMP_ABSOLUTE 8;
  LOAD_FAST 1;
  RETURN_VALUE ;
]

let consts_0x7fb3abf9d0e0 = [
  PYTYP(createNone());
]

let varnames_0x7fb3abf9d0e0 = [
  "obj";
  "obj_elems";
  "elem";
]

let names_0x7fb3abf9d0e0 = [
]

let cellvars_0x7fb3abf9d0e0 = [
]

let freevars_0x7fb3abf9d0e0 = [
]

let co_0x7fb3abf9d0e0 = {
  co_code = bc_0x7fb3abf9d0e0;
  co_consts = consts_0x7fb3abf9d0e0;
  co_varnames = varnames_0x7fb3abf9d0e0;
  co_names = names_0x7fb3abf9d0e0;
  co_cellvars = cellvars_0x7fb3abf9d0e0;
  co_freevars = freevars_0x7fb3abf9d0e0;
}

let consts_0x7fb3abf9d500 = [
  PYTYP(createString "Tests for loops
    ");
  CODEOBJECT(co_0x7fb3abf9d0e0);
  PYTYP(createString "top_level.<locals>.run_for_loop");
  CODEOBJECT(co_0x7fb3abf9d190);
  PYTYP(createString "top_level.<locals>.test_while_loop");
  CODEOBJECT(co_0x7fb3abf9d240);
  PYTYP(createString "top_level.<locals>.test_list_for_loop");
  CODEOBJECT(co_0x7fb3abf9d2f0);
  PYTYP(createString "top_level.<locals>.test_tuple_for_loop");
  CODEOBJECT(co_0x7fb3abf9d3a0);
  PYTYP(createString "top_level.<locals>.test_string_for_loop");
  CODEOBJECT(co_0x7fb3abf9d450);
  PYTYP(createString "top_level.<locals>.test_dict_for_loop");
]

let varnames_0x7fb3abf9d500 = [
  "test_while_loop";
  "test_list_for_loop";
  "test_tuple_for_loop";
  "test_string_for_loop";
  "test_dict_for_loop";
  "result";
]

let names_0x7fb3abf9d500 = [
]

let cellvars_0x7fb3abf9d500 = [
  "run_for_loop";
]

let freevars_0x7fb3abf9d500 = [
]

let co_0x7fb3abf9d500 = {
  co_code = bc_0x7fb3abf9d500;
  co_consts = consts_0x7fb3abf9d500;
  co_varnames = varnames_0x7fb3abf9d500;
  co_names = names_0x7fb3abf9d500;
  co_cellvars = cellvars_0x7fb3abf9d500;
  co_freevars = freevars_0x7fb3abf9d500;
}

let virt_m, res = runCode_returnVM co_0x7fb3abf9d500
let print_program_state = IO.print_string (print_program_state virt_m res)