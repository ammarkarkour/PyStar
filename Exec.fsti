module Exec

(* imported modules *)
open Structs
open Utils
(* ---------------- *)

val makeFrame: vm -> codeObj -> list pyObj -> (Map.t string pyObj) -> (Map.t string pyObj) -> (vm * frameObj)

val call_function:  nat -> (Map.t string pyObj) -> list pyObj -> All.ML (list pyObj)

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

val binary_subscr: list pyObj -> All.ML (list pyObj)

val store_name: nat -> list pyObj -> Map.t string pyObj -> list pyObj -> All.ML (Map.t string pyObj * list pyObj)

val load_const: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val load_name: nat -> list pyObj -> Map.t string pyObj -> Map.t string pyObj -> list pyObj-> All.ML (list pyObj)

val build_tuple: nat -> list pyObj -> All.ML (list pyObj)

val build_list: nat -> list pyObj -> All.ML (list pyObj)

val pop_jump_if_true: nat -> nat -> list pyObj -> All.ML (nat * list pyObj)

val pop_jump_if_false: nat -> nat -> list pyObj -> All.ML (nat * list pyObj)

val jump_if_true_or_pop: nat -> nat -> list pyObj -> All.ML (nat * list pyObj)

val jump_if_false_or_pop: nat -> nat -> list pyObj -> All.ML (nat * list pyObj)

val load_global: nat -> list pyObj -> Map.t string pyObj -> list pyObj-> All.ML (list pyObj)

val load_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val store_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj * list pyObj)

val make_function: nat -> (Map.t string pyObj) ->list pyObj -> All.ML (list pyObj)

val execBytecode: frameObj -> All.ML (frameObj)

val runFrame: vm -> frameObj -> All.ML (vm * pyObj)
