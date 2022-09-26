module PyFunction

open Structs
open Utils

let createFunction (f: functionObj) =
  let lt =
    Map.upd emptyMap "__lt__" 
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in

  let neq =
    Map.upd eq "__ne__"
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in

  let gt =
    Map.upd neq "__gt__"
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in

  let ge =
    Map.upd gt "__ge__"
      (BINFUNBLT (fun (a, b) -> EXCEPTION "Funtion Error")) in
      
  let allMethods = ge in
  let obj: cls = {
    name = "function";
    pid = 0;
    value = FUNCTION(f);
    fields = emptyMap;
    methods = allMethods
  } in obj

