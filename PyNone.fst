module PyNone

open Structs
open Utils

let rec createNone = 

  let bool =
    Map.upd emptyMap "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | NONE -> createBool false
        | _ -> createNone)) in

  (* Should return NotImplemented (Something similar to None) *)
  let lt =
    Map.upd bool "__lt__" 
      (BINFUN (fun (a, b) -> createNone)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->  createNone)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> createBool true 
        | _ -> createNone)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> createBool false
        | _ -> createNone)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> createNone)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) ->  createNone)) in
  let allMethods = ge in
  let obj: type0 = {
    name = "NoneType";
    pid = 0;
    value = NONE;
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)
