module Test 

(* imported modules *)
open Structs
open VM
open Utils
(* ---------------- *)

(* Test case 1*)
let bc1 = CODE [
    LOAD_CONST 1;
    RETURN_VALUE
]

let co_test1 = {
  co_code = bc1;
  co_consts = [NONE; INT 3];
  co_varnames = []
}

(* Test case 2 *)
let bc2 = CODE [
  LOAD_CONST 1;
  STORE_FAST 0;

  LOAD_CONST 2;
  STORE_FAST 1;

  LOAD_FAST 0;
  LOAD_FAST 1;
  BINARY_ADD;
  STORE_FAST 2;

  LOAD_FAST 2;
  RETURN_VALUE
]

let co_test2 = {
  co_code = bc2;
  co_consts = [NONE; INT 1; INT 2];
  co_varnames = [STRING "x"; STRING "y"; STRING "Z"]
}


let res = runCode co_test2

let p = print_pyObj res
  
