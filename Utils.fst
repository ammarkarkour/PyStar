module Utils 

(* imported modules *)
open Structs
(* ---------------- *)


let print_pyObj p =
  match p with
  | INT i -> Printf.sprintf "INT: %d" i
  | STRING s -> Printf.sprintf "STRING: %s" s
  | BOOL b ->  Printf.sprintf "BOOL: %b" b
  | NONE -> Printf.sprintf "NONE"
  | ERR s -> Printf.sprintf "ERR: %s" s
  (*| _ -> Printf.sprintf ""*)
