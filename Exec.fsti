module Exec

(* imported modules *)
open Structs
open Utils
(* ---------------- *)

val makeFrame: vm -> codeObj -> list pyObj -> (Map.t string pyObj) -> (Map.t string pyObj) -> (vm * frameObj)

val pop_top: list pyObj -> All.ML (list pyObj)

val rot_two: list pyObj -> All.ML (list pyObj)

val rot_three: list pyObj -> All.ML (list pyObj)

val rot_four: list pyObj -> All.ML (list pyObj)

val dup_top: list pyObj -> All.ML (list pyObj)

val dup_top_two: list pyObj -> All.ML (list pyObj)

val binary_add: list pyObj -> All.ML (list pyObj)

val store_name: nat -> list string -> Map.t string pyObj -> list pyObj -> All.ML (Map.t string pyObj * list pyObj)

val load_const: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val load_name: nat -> list string -> Map.t string pyObj -> Map.t string pyObj -> list pyObj-> All.ML (list pyObj)

val load_global: nat -> list string -> Map.t string pyObj -> list pyObj-> All.ML (list pyObj)

val load_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj)

val store_fast: nat -> list pyObj -> list pyObj -> All.ML (list pyObj * list pyObj)

val execBytecode: frameObj -> All.ML (frameObj)

val runFrame: vm -> frameObj -> All.ML (vm * pyObj)
