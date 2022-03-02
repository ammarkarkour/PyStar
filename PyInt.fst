module PyInt

(* imported modules *)
open Structs
open Utils
(* ---------------- *)

(* Functions used to create objects of builtin types *)
let createInt (v:int) =

  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUN (fun a -> 
        match pyTypTobuiltins a with 
        | INT(a) -> INT(a)
        | _ -> NONE)) in

  let neg =
    Map.upd pos "__neg__"
      (UNFUN (fun a ->
        match pyTypTobuiltins a with
        | INT(a) -> INT(-a)
        | _ -> NONE)) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | INT(a) ->  BOOL(if a=0 then false else true)
        | _ -> NONE)) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> INT(op_Multiply b a) 
        | INT(a), BOOL(b) -> INT(if b then a else 0)
        | INT(a), STRING(s) ->
          if 0 >= a
          then STRING("")
          else
          STRING(String.concat "" (tabulate (fun x -> s) a))
        | INT(a), LIST(l) -> 
          if 0 >= a
          then LIST([])
          else
          LIST(List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | INT(a), TUPLE(l) ->
          if 0 >= a
          then LIST([])
          else
          LIST(List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> NONE)) in
  
  let floordiv =
    Map.upd mul "__floordiv__" 
            (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then NONE else INT(b/a) 
        | INT(a), BOOL(b) -> if a=0 then NONE else if b then INT(1/a) else INT(0)
        | _ -> NONE)) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then NONE else INT(b%a)
        | INT(a), BOOL(b) -> if a=0 then NONE else if b then INT(1%a) else INT(0)
        | _ -> NONE)) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> INT(a + b) 
        | INT(a), BOOL(b) -> INT(if b then 1+a else a)
        | _ -> NONE)) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> INT(b - a) 
        | INT(a), BOOL(b) -> INT(if b then 1+a else a)
        | _ -> NONE)) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> BOOL(a < b) 
        | INT(a), BOOL(b) -> BOOL(if b then a<1 else a<0)
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> BOOL(a <= b) 
        | INT(a), BOOL(b) -> BOOL(if b then a<=1 else a<=0)
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | INT(a), INT(b) -> BOOL(a = b) 
        | INT(a), BOOL(b) -> BOOL(if b then a=1 else a=0)
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> BOOL(a <> b) 
        | INT(a), BOOL(b) -> BOOL(if b then a<>1 else a<>0)
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> BOOL(a > b) 
        | INT(a), BOOL(b) -> BOOL(if b then a>1 else a>0)
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> BOOL(a >= b) 
        | INT(a), BOOL(b) -> BOOL(if b then a>=1 else a>=0)
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0= {
    name = "int";
    pid = 0;
    value = INT(v);
    fields = emptyMap;
    methods = allMethods;
  } in
  PYTYP(OBJ(obj))
