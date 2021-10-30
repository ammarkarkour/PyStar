module VM

(* imported modules *)
open Structs
(* ---------------- *)

(* Creates a new frame and push it to the frame stack *)
val makeFrame: vm -> codeObj ->  (vm * frameObj)

(* Run Frame and then pop it from call stack when you done *)
(* val runFrame: vm -> frameObj -> All.ML (vm * pyObj) *)
val runFrame: vm -> frameObj -> All.ML pyObj

(* Takes in code object and execute it *)
val runCode: codeObj -> All.ML pyObj

