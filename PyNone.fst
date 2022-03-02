module PyNone

open Structs
open Utils

let createNone = 

  let bool =
    Map.upd emptyMap "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | NONE -> BOOL(false)
        | _ -> NONE)) in

  (* Should return NotImplemented (Something similar to None) *)
  let lt =
    Map.upd bool "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> BOOL(true) 
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> BOOL(false)
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0 = {
    name = "NoneType";
    pid = 0;
    value = NONE;
    fields = emptyMap;
    methods = allMethods
  } in
  PYTYP(OBJ(obj))
