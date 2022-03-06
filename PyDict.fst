module PyDict

(* imported modules *)
open Structs
open Utils
open PyList
(* ---------------- *)


(*
   - Return true if k is hashable
*)
let is_hashable_key (k: pyTyp) = false

(*
  - Returns true if each element in kl is hashable
*)
let rec is_hashable (kl: list pyTyp) = 
  match kl with
  | [] -> true
  | k::kl' -> (is_hashable_key k) && (is_hashable kl')

let createDict (kvl: list (pyTyp * pyTyp)) = 
  let kl, vl = List.Tot.Base.unzip kvl in
  match is_hashable kl with
  | false -> ERR("Key is mot hashable")
  | true ->

    let contains =
      Map.upd emptyMap "__contains__" 
        (BINFUN (fun (a, b) ->
          match a with
          | OBJ(obja) -> 
            (match Map.sel (obja.fields) "keys" with
            | PYTYP(objakl) ->
              (match pyTypTobuiltins objakl with
              | LIST(kl) -> BOOL(list_contains kl b)
              | _ -> NONE)
            | _ -> NONE )  
          | _ -> NONE)) in

    let lt =
    Map.upd contains "__lt__" 
      (BINFUN (fun (a, b) ->  NONE)) in
        
    let le =
      Map.upd lt "__le__" 
        (BINFUN (fun (a, b) -> NONE)) in
    
    let eq =
      Map.upd le "__eq__" 
        (BINFUN (fun (a, b) -> 
          match (pyTypTobuiltins a, pyTypTobuiltins b) with 
          | DICT(kvl1), DICT(kvl2) ->
            let kvl1_keys, kvl1_vals = List.Tot.Base.unzip kvl1 in
            let kvl2_keys, kvl2_vals = List.Tot.Base.unzip kvl2 in
            (match list_lex_eq kvl1_keys kvl2_keys, list_lex_eq kvl1_vals kvl2_vals with
            | BOOL(b1), BOOL(b2) -> BOOL(b1 && b2)
            | _ -> BOOL(false))  
          | _ -> NONE)) in

    let neq =
      Map.upd eq "__ne__" 
        (BINFUN (fun (a, b) -> 
          match (pyTypTobuiltins a, pyTypTobuiltins b) with 
          | DICT(kvl1), DICT(kvl2) ->
            let kvl1_keys, kvl1_vals = List.Tot.Base.unzip kvl1 in
            let kvl2_keys, kvl2_vals = List.Tot.Base.unzip kvl2 in
            (match list_lex_ne kvl1_keys kvl2_keys, list_lex_ne kvl1_vals kvl2_vals with
            | BOOL(b1), BOOL(b2) -> BOOL(b1 && b2)
            | _ -> BOOL(false))  
          | _ -> NONE)) in

    let gt =
      Map.upd neq "__gt__" 
        (BINFUN (fun (a, b) -> NONE)) in

    let ge =
      Map.upd gt "__ge__" 
        (BINFUN (fun (a, b) -> NONE)) in

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
