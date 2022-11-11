module PyNone

open Structs
open Utils

let createNone () = 

  let bool =
    Map.upd emptyMap "__bool__" 
      (UNFUNBLT (fun a ->
        match a.value with 
        | NONE -> BOOL false
        | _ -> EXCEPTION "None Error")) in

  (* Should return NotImplemented (Something similar to None) *)
  let lt =
    Map.upd bool "__lt__" 
      (BINFUNBLT (fun (a, b) -> EXCEPTION "None Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) ->  EXCEPTION "None Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | NONE, NONE -> BOOL true 
        | _ -> EXCEPTION "None Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | NONE, NONE -> BOOL false
        | _ -> EXCEPTION "None Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) -> EXCEPTION "None Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) ->  EXCEPTION "None Error")) in
  let allMethods = ge in
  let obj: cls = {
    name = "NoneType";
    pid = 0;
    value = NONE;
    fields = emptyMap;
    methods = allMethods
  } in obj
