module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f5b70673710 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  COMPARE_OP 2;
  POP_JUMP_IF_TRUE 12;
  LOAD_GLOBAL 0;
  RAISE_VARARGS 1;
  LOAD_CONST 3;
  RETURN_VALUE ;
]

let consts_0x7f5b70673710 = [
  PYTYP(createString "Write your own code inside this function
    ");
  PYTYP(createInt 1);
  PYTYP(createInt 2);
  PYTYP(createNone());
]

let varnames_0x7f5b70673710 = [
]

let names_0x7f5b70673710 = [
  "AssertionError";
]

let cellvars_0x7f5b70673710 = [
]

let freevars_0x7f5b70673710 = [
]

let co_0x7f5b70673710 = {
  co_code = bc_0x7f5b70673710;
  co_consts = consts_0x7f5b70673710;
  co_varnames = varnames_0x7f5b70673710;
  co_names = names_0x7f5b70673710;
  co_cellvars = cellvars_0x7f5b70673710;
  co_freevars = freevars_0x7f5b70673710;
}

let virt_m, res = runCode_returnVM co_0x7f5b70673710
let print_program_state = IO.print_string (print_program_state virt_m res)