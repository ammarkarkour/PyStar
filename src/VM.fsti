module VM

(* imported modules *)
open Utils
open Structs
open Exec
(* ---------------- *)

(* Takes in code object and execute it *)
val runCode: codeObj -> All.ML pyObj

