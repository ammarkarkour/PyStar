module Minimal

(* Generates weird error message *)
(*
let rec f = 
  let g = f in 4
*)

let rec f (x: int)= 
  (* let g = f 0 in 0  -- It's ok that this does not work *)
  let g = (fun y -> match y with | 0 -> f y | _ -> f y) in 5 (* Does not work *)
  (* Works *)
(*  let g = (fun y -> if y = 0 then f y else f y) in 5*)
(*  let g = (fun y -> f y) in 5*)
