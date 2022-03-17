module PyInt

(* imported modules *)
open Structs
open Utils
(* ---------------- *)

(* Functions used to create objects of builtin types *)
let rec createInt (v:int) =

  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUN (fun a -> 
        match pyTypTobuiltins a with 
        | INT(a) -> createInt a
        | _ -> createNone)) in

  let neg =
    Map.upd pos "__neg__"
      (UNFUN (fun a ->
        match pyTypTobuiltins a with
        | INT(a) -> createInt (-a)
        | _ -> createNone)) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | INT(a) ->  createBool (if a=0 then false else true)
        | _ -> createNone)) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (op_Multiply b a) 
        | INT(a), BOOL(b) -> createInt (if b then a else 0)
        | INT(a), STRING(s) ->
          if 0 >= a
          then createString ("")
          else
          createString (String.concat "" (tabulate (fun x -> s) a))
        | INT(a), LIST(l) -> 
          if 0 >= a
          then createList []
          else
          createList (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | INT(a), TUPLE(l) ->
          if 0 >= a
          then createTuple []
          else
          createTuple (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> createNone)) in
  
  let floordiv =
    Map.upd mul "__floordiv__" 
            (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then createNone else createInt (b/a) 
        | INT(a), BOOL(b) -> if a=0 then createNone else if b then createInt (1/a) else createInt 0
        | _ -> createNone)) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then createNone else createInt (b%a)
        | INT(a), BOOL(b) -> if a=0 then createNone else if b then createInt (1%a) else createInt 0
        | _ -> createNone)) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (a + b) 
        | INT(a), BOOL(b) -> createInt (if b then 1+a else a)
        | _ -> createNone)) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (b - a) 
        | INT(a), BOOL(b) -> createInt (if b then 1+a else a)
        | _ -> createNone)) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (a < b) 
        | INT(a), BOOL(b) -> createInt (if b then a<1 else a<0)
        | _ -> createNone)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a <= b) 
        | INT(a), BOOL(b) -> createBool (if b then a<=1 else a<=0)
        | _ -> createNone)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | INT(a), INT(b) -> createBool (a = b) 
        | INT(a), BOOL(b) -> createBool (if b then a=1 else a=0)
        | _ -> createNone)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a <> b) 
        | INT(a), BOOL(b) -> createBool (if b then a<>1 else a<>0)
        | _ -> createNone)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a > b) 
        | INT(a), BOOL(b) -> createBool (if b then a>1 else a>0)
        | _ -> createNone)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a >= b) 
        | INT(a), BOOL(b) -> createBool (if b then a>=1 else a>=0)
        | _ -> createNone)) in
  let allMethods = ge in
  let obj: type0= {
    name = "int";
    pid = 0;
    value = INT(v);
    fields = emptyMap;
    methods = allMethods;
  } in
  OBJ(obj)
