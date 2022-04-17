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
    | PYTYP(obj) -> 
      (match obj.value with
      | EXCEPTION e -> Some (PYTYP obj)
      | _ -> None)
    | _ -> None

let rec print_type0 (t: cls): All.ML string = print_builtin t.value

and print_builtin (b: builtins): All.ML string =
  match b with
  | INT i -> Printf.sprintf "INT: %d" i
  | STRING s -> Printf.sprintf "STRING: %s" s
  | BOOL b ->  Printf.sprintf "BOOL: %b" b
  | LIST l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_type0 l) ""
  | TUPLE l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_type0 l) ""
  | NONE -> Printf.sprintf "NONE"
  | _ -> Printf.sprintf "Not printable"
  
let print_pyObj (p:pyObj): All.ML string =
  match p with
  | ERR s -> Printf.sprintf "ERR: %s" s
  | PYTYP (typ) ->  print_type0 typ
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

let tabulate (#a:Type) (f: nat -> a) (i: nat): (list a) =
  List.rev (tabulate' f i)

let rec listToPairs (l:list 'a {(List.Tot.length l)%2 = 0}): Tot (list ('a * 'a)) =
  match l with
  | [] -> []
  | x::y::l2 -> (x, y)::(listToPairs l2)
  
let pyObjToobj (p: pyObj {PYTYP? p}): cls = 
  match p with
  | PYTYP(obj) -> obj


let rec unwrapPyObjList (l: list pyObj): option (list cls) =
  match l with
  | [] -> Some []
  | x::l ->
    (match x with
    | PYTYP obj -> 
      let l2 = unwrapPyObjList l in
      (match l2 with
      | None -> None 
      | Some l3 -> Some (obj::l3))
    | _ -> None)

val totZip: (l1: list 'a)  -> (l2: list 'b {List.Tot.length l1 = List.Tot.length l2}) -> Tot (list ('a * 'b))
let rec totZip l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | hd1::tl1, hd2::tl2 -> (hd1,hd2)::(totZip tl1 tl2)


(* Equality defined for Python objects *)
let objEq (obj1: cls) (obj2: cls): bool = 
  match (Map.sel (obj1.methods) "__eq__") with
  | BINFUNBLT f ->
    (match f (obj1, obj2) with
    | BOOL b -> b
    | _ -> false)
  | _ -> false

let rec list_contains (l: list cls) (x: cls) =
  match l with
  | [] -> false
  | h::l -> (objEq x h) || (list_contains l x)

(*-----------------------------------------------------------*)
(*-------------- Python lex List Comparision ----------------*)
(*-----------------------------------------------------------*)
(* NOTE: Will change once exceptions are impelemnted *)

let rec list_lex_lt l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(false)
  | [], h2::l2 -> BOOL(true)
  | h1::l1, [] -> BOOL(false)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__ge__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_lt l1 l2
        | true -> BOOL(false))

      | _ -> NONE)
    | err -> NONE)

let rec list_lex_le l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(true)
  | [], h2::l2 -> BOOL(true)
  | h1::l1, [] -> BOOL(false)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__gt__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_le l1 l2
        | true -> BOOL(false))
      | _ -> NONE)
    | err -> NONE)

let rec list_lex_eq l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(true)
  | [], h2::l2 -> BOOL(false)
  | h1::l1, [] -> BOOL(false)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__ne__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_eq l1 l2
        | true -> BOOL(false))
      | _ -> NONE)
    | err -> NONE)

let rec list_lex_ne l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(false)
  | [], h2::l2 -> BOOL(true)
  | h1::l1, [] -> BOOL(true)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__eq__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_ne l1 l2
        | true -> BOOL(false))
      | _ -> NONE)
    | err -> NONE)

let rec list_lex_gt l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(false)
  | [], h2::l2 -> BOOL(false)
  | h1::l1, [] -> BOOL(true)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__le__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_gt l1 l2
        | true -> BOOL(false))
      | _ -> NONE)
    | err -> NONE)

let rec list_lex_ge l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(true)
  | [], h2::l2 -> BOOL(false)
  | h1::l1, [] -> BOOL(true)
  | h1::l1, h2::l2 -> 
    (match (Map.sel (h1.methods) "__lt__") with
    | BINFUNBLT f -> 
      (match f (h1, h2) with
      | NONE -> NONE
      | BOOL b ->
        (match b with
        | false -> list_lex_ge l1 l2
        | true -> BOOL(false))
      | _ -> NONE)
    | err -> NONE)

(*----------------------------------------------------------*)

(* Helper functions to create objects of builtin type *)
let emptyMap: Map.t string pyObj  = Map.const (ERR("UNDEFINED"))

(* Err raised when the interprter conduct an undefined behavior *)
let undefinedBehavior s =  ERR (String.concat "" ["INTERPRTER UNDEFINED BEHAVIOR "; s])

(* 
  - objects ids, which will be filled during compiling time
  - key: *)
let idsMap: Map.t hashable nat  = Map.const 0

(* A counter that will be used to assign process ids for new objects *)
// let pids = FStar.Ref.alloc 1 
// let res:int = FStar.Ref.read pids
// let _ = FStar.Ref.write pids ((FStar.Ref.read pids) + 1)  

