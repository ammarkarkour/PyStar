module Utils

(* imported modules *)
open Structs
(* ---------------- *)

(*
   Req: true
*)
let check_err dataStack = 
  match (List.length dataStack) with
  | 0 -> None
  | _ ->
    match (List.hd dataStack) with
    | ERR(s) -> Some (ERR(s))
    | _ -> None
    
let rec print_pyObj p: All.ML string =
  match p with
  | INT i -> Printf.sprintf "INT: %d" i
  | STRING s -> Printf.sprintf "STRING: %s" s
  | BOOL b ->  Printf.sprintf "BOOL: %b" b
  | LIST l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_pyObj l) ""
  | TUPLE l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_pyObj l) ""
  | NONE -> Printf.sprintf "NONE"
  | ERR s -> Printf.sprintf "ERR: %s" s
  | _ -> Printf.sprintf "Not printable"

let rec subString_pos' (cl: list String.char) (i: int): All.ML (option String.char) =
  match cl with
  | [] -> None
  | x::cl' -> if i=0 then Some x else if i < 0 then None else (subString_pos' cl' (i-1))

let subString_pos (s: string) (i: int): All.ML (option string) =
  match (subString_pos' (String.list_of_string s) i) with
  | None -> None
  | Some c -> Some (String.string_of_char c)

let subString_neg (s: string) (i: int): All.ML (option string) = subString_pos s ((String.length s) + i)

let rec tabulate' (#a:Type) (f: nat -> a) (i: nat): list a =
  match i with
  | 0 -> []
  | i -> (f (i-1))::(tabulate' f (i-1))

let rec tabulate (#a:Type) (f: nat -> a) (i: nat): list a =
  List.rev (tabulate' f i)
