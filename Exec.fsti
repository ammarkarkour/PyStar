module Exec

(* imported modules *)
open Structs
(* ---------------- *)

val return_value: list pyObj -> All.ML pyObj

val load_const: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val load_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val store_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj * list pyObj)

val binary_add: list pyObj -> All.ML (list pyObj)

val execBytecode: frameObj -> All.ML pyObj
