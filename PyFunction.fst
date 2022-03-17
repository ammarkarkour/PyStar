module PyFunction

open Structs
open Utils

let createFunction (f: functionObj) =


  let lt =
    Map.upd emptyMap "__lt__" 
      (BINFUN (fun (a, b) -> createNone)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> createNone)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> createNone)) in

  let neq =
    Map.upd eq "__ne__"
      (BINFUN (fun (a, b) -> createNone)) in

  let gt =
    Map.upd neq "__gt__"
      (BINFUN (fun (a, b) -> createNone)) in

  let ge =
    Map.upd gt "__ge__"
      (BINFUN (fun (a, b) -> createNone)) in
  let allMethods = ge in
  let obj: type0= {
    name = "function";
    pid = 0;
    value = FUNCTION(f);
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)

