module Exec

(* imported modules *)
open Structs
(* ---------------- *)

val check_err: list pyObj -> All.ML (option pyObj) 

val return_value: list pyObj -> All.ML pyObj

val pop_top: list pyObj -> All.ML (list pyObj)

val rot_two: list pyObj -> All.ML (list pyObj)

val rot_three: list pyObj -> All.ML (list pyObj)

val rot_four: list pyObj -> All.ML (list pyObj)

val dup_top: list pyObj -> All.ML (list pyObj)

val dup_top_two: list pyObj -> All.ML (list pyObj)

val unary_positive: list pyObj -> All.ML (list pyObj)

val unary_negative: list pyObj -> All.ML (list pyObj)

val unary_not: list pyObj -> All.ML (list pyObj)

val binary_multiply: list pyObj -> All.ML (list pyObj)

val binary_floor_divide: list pyObj -> All.ML (list pyObj)

val binary_modulo: list pyObj -> All.ML (list pyObj)

val binary_add: list pyObj -> All.ML (list pyObj)

val binary_subtract: list pyObj -> All.ML (list pyObj)

val load_const: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val load_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val store_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj * list pyObj)

val execBytecode: frameObj -> All.ML pyObj
