module PyBool

open Structs
open Utils

let rec createBool b =

  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUN (fun a -> 
        match pyTypTobuiltins a with 
        | BOOL(b) -> createInt (if b then 1 else 0)
        | _ -> createNone)) in

  let neg =
    Map.upd pos "__neg__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | BOOL b -> createInt (if b then -1 else 0)
        | _ -> createNone)) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | BOOL(b) ->  createInt b
        | _ -> createNone)) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 1 else 0)
        | BOOL(b), INT(i) -> createInt (if b then i else  0)
        | BOOL(b), LIST(l) -> if b then createList l else createList []
        | BOOL(b), TUPLE(t) -> if b then createTuple t else createTuple []
        | BOOL(b), STRING(s) -> if b then createString s else createString ""
        | _ -> createNone)) in
        
  (* createNone should be createExp *)
  let floordiv =
    Map.upd mul "__floordiv__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> if not b1 then createNone else if b2 then createInt 1 else createInt 0
        | BOOL(b1), INT(i) -> if not b1 then createNone else createInt (i/1)
        | _ -> createNone)) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> if not b1 then createNone else createInt 0
        | BOOL(b1), INT(i) -> if not b1 then createNone else createInt 0
        | _ -> createNone)) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 2 else if b1||b2 then 1 else 0)
        | BOOL(b1), INT(i) -> createInt (if b1 then 1+i else  i)
        | _ -> createNone)) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 0 else if b1 then -1 else if b2 then 1 else 0)
        | BOOL(b1), INT(i) -> createInt (if b1 then i-1 else  i)
        | _ -> createNone)) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then false else b2)
        | BOOL(b), INT(i) -> createBool (if b then 1<i else 0<i)
        | _ -> createNone)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then b2 else true)
        | BOOL(b), INT(i) -> createBool (if b then 1<=i else 0<=i)
        | _ -> createNone)) in

  let eq =
    Map.upd le "__eq__"
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (b1=b2)
        | BOOL(b), INT(i) -> createBool (if b then i=1 else i=0)
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
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then not b2 else false)
        | BOOL(b), INT(i) -> createBool (if b then 1>i else 0>i)
        | _ -> createNone)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then true else not b2)
        | BOOL(b), INT(i) -> createBool (if b then 1>=i else 0>=i)
        | _ -> createNone)) in
  let allMethods = ge in
  let obj: type0= {
    name = "NoneType";
    pid = 0;
    value = BOOL(b);
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)
