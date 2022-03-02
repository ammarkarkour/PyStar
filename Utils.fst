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

let rec print_pyTyp (t: pyTyp): All.ML string =
  match t with
  | OBJ(obj) -> print_builtin obj.value

and print_builtin (b: builtins): All.ML string =
  match b with
  | INT i -> Printf.sprintf "INT: %d" i
  | STRING s -> Printf.sprintf "STRING: %s" s
  | BOOL b ->  Printf.sprintf "BOOL: %b" b
  | LIST l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_pyTyp l) ""
  | TUPLE l -> List.fold_right (fun a b -> a ^ " " ^ b) (List.map print_pyTyp l) ""
  | NONE -> Printf.sprintf "NONE"
  | _ -> Printf.sprintf "Not printable"
  
let print_pyObj (p:pyObj): All.ML string =
  match p with
  | ERR s -> Printf.sprintf "ERR: %s" s
  | PYTYP (typ) ->  print_pyTyp typ
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

let tabulate (#a:Type) (f: nat -> a) (i: nat): list a =
  List.rev (tabulate' f i)

let pyTypTobuiltins (t: pyTyp) =
  match t with
  | OBJ(obj) -> obj.value

let pyObjTopyTyp (p: pyObj): All.ML pyTyp = 
  match p with
  | PYTYP(obj) -> obj
  | _ -> All.failwith "Expected Python object, but found interpter object"
  
(* Equality defined for Python objects *)
let pyTypEq (t1: pyTyp) (t2: pyTyp): bool =
  match (t1, t2) with
  | OBJ(obj1), OBJ(obj2) -> 
    (match (Map.sel (obj1.methods) "__eq__") with
    | BINFUN f ->
      (match f (t1, t2) with
      | NONE -> false
      | bltin -> 
        (match bltin with
        | BOOL(b) -> b
        | _ -> false))
    | _ -> false)
  | _ -> false

let rec list_contains (l: list pyTyp) (x: pyTyp) =
  match l with
  | [] -> false
  | h::l -> (pyTypEq x h) || (list_contains l x)

(*-----------------------------------------------------------*)
(*-------------- Python lex List Comparision ----------------*)
(*-----------------------------------------------------------*)

let rec list_lex_lt l1 l2 = 
  match l1, l2 with
  | [], [] -> BOOL(false)
  | [], h2::l2 -> BOOL(true)
  | h1::l1, [] -> BOOL(false)
  | h1::l1, h2::l2 -> 
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__ge__") with
      | BINFUN f -> 
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
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__gt__") with
      | BINFUN f -> 
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
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__ne__") with
      | BINFUN f -> 
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
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__eq__") with
      | BINFUN f -> 
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
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__le__") with
      | BINFUN f -> 
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
    match h1 with
    | OBJ(obj1) ->
      (match (Map.sel (obj1.methods) "__lt__") with
      | BINFUN f -> 
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

(* 
  - objects ids, which will be filled during compiling time
  - key: *)
let idsMap: Map.t string int  = Map.const 0

(* A counter that will be used to assign process ids for new objects *)
// let pids = FStar.Ref.alloc 1 
// let res:int = FStar.Ref.read pids
// let _ = FStar.Ref.write pids ((FStar.Ref.read pids) + 1)










  
