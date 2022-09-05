module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f212d1ad0e0 = CODE [
  LOAD_CONST 0;
  LOAD_CONST 1;
  MAKE_FUNCTION 0;
  STORE_NAME 0;
  LOAD_CONST 2;
  LOAD_CONST 3;
  MAKE_FUNCTION 0;
  STORE_NAME 1;
  LOAD_CONST 4;
  RETURN_VALUE None;
]

let bc_0x7f212d1ad030 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE None;
]

let consts_0x7f212d1ad030 = [
  PYTYP(createNone());
  PYTYP(createInt 7);
]

let varnames_0x7f212d1ad030 = [
  "y";
]

let names_0x7f212d1ad030 = [
]

let co_0x7f212d1ad030 = {
  co_code = bc_0x7f212d1ad030;
  co_consts = consts_0x7f212d1ad030;
  co_varnames = varnames_0x7f212d1ad030;
  co_names = names_0x7f212d1ad030
}

let bc_0x7f212d1a9ea0 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;
  LOAD_FAST 0;
  RETURN_VALUE None;
]

let consts_0x7f212d1a9ea0 = [
  PYTYP(createNone());
  PYTYP(createInt 5);
]

let varnames_0x7f212d1a9ea0 = [
  "x";
]

let names_0x7f212d1a9ea0 = [
]

let co_0x7f212d1a9ea0 = {
  co_code = bc_0x7f212d1a9ea0;
  co_consts = consts_0x7f212d1a9ea0;
  co_varnames = varnames_0x7f212d1a9ea0;
  co_names = names_0x7f212d1a9ea0
}

let consts_0x7f212d1ad0e0 = [
  CODEOBJECT(co_0x7f212d1a9ea0);
  PYTYP(createString first_function);
  CODEOBJECT(co_0x7f212d1ad030);
  PYTYP(createString second_function);
  PYTYP(createNone());
]

let varnames_0x7f212d1ad0e0 = [
]

let names_0x7f212d1ad0e0 = [
  "first_function";
  "second_function";
]

let co_0x7f212d1ad0e0 = {
  co_code = bc_0x7f212d1ad0e0;
  co_consts = consts_0x7f212d1ad0e0;
  co_varnames = varnames_0x7f212d1ad0e0;
  co_names = names_0x7f212d1ad0e0
}

let res = runCode co_0x7f212d1ad0e0