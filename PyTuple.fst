module PyTuple

open Structs
open Utils

let createTuple (l: list pyTyp) = 
        
  let mul =
    Map.upd emptyMap "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l), BOOL(b) -> if b then TUPLE(l) else TUPLE([])
        | TUPLE(l), INT(a) -> 
          if 0 >= a
          then TUPLE([])
          else
          TUPLE(List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> NONE)) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> TUPLE(List.append l1 l2)
        | _ -> NONE)) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, b) with 
        | TUPLE(l1), obj -> BOOL(list_contains l1 obj) 
        | _ -> NONE)) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_lt l1 l2
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_le l1 l2
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_eq l1 l2
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_ne l1 l2
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_gt l1 l2
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> list_lex_ge l1 l2
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0= {
    name = "tuple";
    pid = 0;
    value = TUPLE(l);
    fields = emptyMap;
    methods = add
  } in
  PYTYP(OBJ(obj))
