module PyDict

(* imported modules *)
open Structs
open Utils
open PyList
open PyException
(* ---------------- *)

let rec is_hashable_tuple (l: list cls) =
  match l with
  | [] -> true
  | x::l2 -> (is_hashable_key x) && (is_hashable_tuple l2)

and is_hashable_key (k: cls) =
  match k.value with
  | BOOL b -> true
  | STRING s -> true
  | INT i -> true
  | TUPLE l -> is_hashable_tuple l
  | USERDEF ->
    (match (Map.sel (k.methods) "__hash__") with
    | ERR err -> false
    | _ ->
      (match (Map.sel (k.methods) "__eq__") with
      | ERR err -> false
      | _ -> true))
  | _ -> false

(*
  - Returns true if each element in kl is hashable
*)
let rec is_hashable (kl: list cls) = 
  match kl with
  | [] -> true
  | k::kl' -> (is_hashable_key k) && (is_hashable kl')

let createDict (vkl: list (cls * cls)) = 
  let vl, kl = List.Tot.Base.unzip vkl in
  match is_hashable kl with
  | false -> createException "Key is not hashable"
  | true ->

    let contains =
      Map.upd emptyMap "__contains__" 
        (BINFUNBLT (fun (a, b) ->
          (match Map.sel (a.fields) "keys" with
          | PYTYP(objakl) ->
            (match objakl.value with
            | LIST(kl) -> BOOL(list_contains kl b)
            | _ -> EXCEPTION "Dictionary Error")
          | _ -> EXCEPTION "Dictionary Error" ))) in

    let lt =
    Map.upd contains "__lt__" 
      (BINFUNBLT (fun (a, b) ->  EXCEPTION "Dictionary Error")) in
        
    let le =
      Map.upd lt "__le__" 
        (BINFUNBLT (fun (a, b) -> EXCEPTION "Dictionary Error")) in
    
    let eq =
      Map.upd le "__eq__" 
        (BINFUNBLT (fun (a, b) -> 
          match (a.value, b.value) with 
          | DICT(vkl1), DICT(vkl2) ->
            let vkl1_vals, vkl1_keys = List.Tot.Base.unzip vkl1 in
            let vkl2_vals, vkl2_keys = List.Tot.Base.unzip vkl2 in
            (match list_lex_eq vkl1_keys vkl2_keys, list_lex_eq vkl1_vals vkl2_vals with
            | BOOL(b1), BOOL(b2) -> BOOL(b1 && b2)
            | _ -> BOOL(false))
          | _ -> EXCEPTION "Dictionary Error")) in

    let neq =
      Map.upd eq "__ne__" 
        (BINFUNBLT (fun (a, b) -> 
          match (a.value, b.value) with 
          | DICT(vkl1), DICT(vkl2) ->
            let vkl1_vals, vkl1_keys = List.Tot.Base.unzip vkl1 in
            let vkl2_vals, vkl2_keys = List.Tot.Base.unzip vkl2 in
            (match list_lex_ne vkl1_keys vkl2_keys, list_lex_ne vkl1_vals vkl2_vals with
            | BOOL(b1), BOOL(b2) -> BOOL(b1 && b2)
            | _ -> BOOL(false))  
          | _ -> EXCEPTION "Dictionary Error")) in

    let gt =
      Map.upd neq "__gt__" 
        (BINFUNBLT (fun (a, b) -> EXCEPTION "Dictionary Error")) in

    let ge =
      Map.upd gt "__ge__" 
        (BINFUNBLT (fun (a, b) -> EXCEPTION "Dictionary Error")) in

    let subscr =
      Map.upd ge "__subscr__"
        (BINFUNBLT (fun (a, b) ->
          match (a.value, b.value) with
          | DICT(vkl1), key ->
            (match List.Tot.Base.find (fun x -> match x with | v, k -> objEq b k) vkl1 with
            | None -> EXCEPTION "Dictionary Error"
            | Some (v, k) -> k.value)
          | _ -> EXCEPTION "Dictionary Error")) in

    let keys = Map.upd emptyMap "keys" (PYTYP(createList kl)) in
    let values = Map.upd keys "values" (PYTYP(createList vl)) in
    let obj: cls = {
      name = "dict";
      pid = 0;
      value = DICT(vkl);
      fields = values;
      methods = subscr
    } in
    obj
