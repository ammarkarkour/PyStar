module Test

(* imported modules *)
open Structs
open VM
open Utils
open PyBuiltinObjects
(* ---------------- *)

let bc_0x7f6a154adea0 = CODE [
  LOAD_CONST 1;
  LOAD_CONST 2;
  MAKE_FUNCTION 0;
  STORE_FAST 0;
  LOAD_FAST 0;
  LOAD_CONST 3;
  CALL_FUNCTION 1;
  RETURN_VALUE ;
]

let bc_0x7f6a154addf0 = CODE [
  LOAD_FAST 0;
  LOAD_CONST 1;
  COMPARE_OP 4;
  POP_JUMP_IF_FALSE 18;
  LOAD_FAST 0;
  LOAD_CONST 2;
  INPLACE_SUBTRACT ;
  STORE_FAST 0;
  JUMP_ABSOLUTE 0;
  LOAD_FAST 0;
  RETURN_VALUE ;
]

let consts_0x7f6a154addf0 = [
  PYTYP(createNone());
  PYTYP(createInt 0);
  PYTYP(createInt 1);
]

let varnames_0x7f6a154addf0 = [
  "n";
]

let names_0x7f6a154addf0 = [
]

let cellvars_0x7f6a154addf0 = [
]

let freevars_0x7f6a154addf0 = [
]

let co_0x7f6a154addf0 = {
  co_code = bc_0x7f6a154addf0;
  co_consts = consts_0x7f6a154addf0;
  co_varnames = varnames_0x7f6a154addf0;
  co_names = names_0x7f6a154addf0;
  co_cellvars = cellvars_0x7f6a154addf0;
  co_freevars = freevars_0x7f6a154addf0;
}

let consts_0x7f6a154adea0 = [
  PYTYP(createString "Testing running time for while loop 
    ");
  CODEOBJECT(co_0x7f6a154addf0);
  PYTYP(createString "top_level.<locals>.test_while_loop");
  PYTYP(createInt 1000);
]

let varnames_0x7f6a154adea0 = [
  "test_while_loop";
]

let names_0x7f6a154adea0 = [
]

let cellvars_0x7f6a154adea0 = [
]

let freevars_0x7f6a154adea0 = [
]

let co_0x7f6a154adea0 = {
  co_code = bc_0x7f6a154adea0;
  co_consts = consts_0x7f6a154adea0;
  co_varnames = varnames_0x7f6a154adea0;
  co_names = names_0x7f6a154adea0;
  co_cellvars = cellvars_0x7f6a154adea0;
  co_freevars = freevars_0x7f6a154adea0;
}

let virt_m, res = runCode_returnVM co_0x7f6a154adea0
let print_program_state = IO.print_string (print_program_state virt_m res)