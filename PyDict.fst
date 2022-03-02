module PyDict

(* imported modules *)
open Structs
open Utils
open PyList
(* ---------------- *)


(*
 - Note: Dictionaries are not fully supported yet
*)



let is_hashable_key (k: pyTyp) = false

(*
  - Returns true if k 
*)
let rec is_hashable (kl: list pyTyp) = 
  match kl with
  | [] -> true
  | k::kl' -> (is_hashable_key k) || (is_hashable kl')

let createDict (kvl: list (pyTyp * pyTyp)) = 
  let kl, vl = List.Tot.Base.unzip kvl in
  match is_hashable kl with
  | false -> ERR("Key is mot hashable")
  | true ->
    let keys = Map.upd emptyMap "keys" (createList kl) in
    let values = Map.upd keys "values" (createList vl) in
    let obj: type0 = {
      name = "dict";
      pid = 0;
      value = DICT(kvl);
      fields = values;
      methods = emptyMap 
    } in
    PYTYP(OBJ(obj))

