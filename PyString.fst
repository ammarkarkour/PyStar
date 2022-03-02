module PyString

open Structs
open Utils

let createString (s: string) =
        
  let mul =
    Map.upd emptyMap "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s), BOOL(b) -> if b then STRING(s) else STRING("")
        | STRING(s), INT(a) ->
          if 0 >= a
          then STRING("")
          else
          STRING(String.concat "" (tabulate (fun x -> s) a))
        | _ -> NONE)) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s1), STRING(s2) -> STRING(s2 ^  s1)
        | _ -> NONE)) in

  let lt =
    Map.upd add "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(false)
          | _ -> BOOL(true)) 
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(false)
          | _ -> BOOL(true))
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(false)
          | _ -> BOOL(false))
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(true)
          | _ -> BOOL(true))
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(true)
          | _ -> BOOL(false))
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(true)
          | _ -> BOOL(false))
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0 = {
    name = "str";
    pid = 0;
    value = STRING s;
    fields = emptyMap;
    methods = allMethods
  } in
  PYTYP(OBJ(obj))
