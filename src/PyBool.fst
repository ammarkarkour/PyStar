module PyBool
  
open Structs
open Utils

let createBool b =
  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUNBLT (fun a -> 
        match a.value with 
        | BOOL(b) -> INT (if b then 1 else 0)
        | _ -> EXCEPTION "Bool Error")) in

  let neg =
    Map.upd pos "__neg__" 
      (UNFUNBLT (fun a ->
        match a.value with 
        | BOOL b -> INT (if b then -1 else 0)
        | _ -> EXCEPTION "Bool Error")) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUNBLT (fun a ->
        match a.value with 
        | BOOL(b) ->  BOOL b
        | _ -> EXCEPTION "Bool Error")) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | BOOL(b1), BOOL(b2) -> INT (if b1&&b2 then 1 else 0)
        | BOOL(b), INT(i) -> INT (if b then i else  0)
        | BOOL(b), LIST(l) -> if b then LIST l else LIST []
        | BOOL(b), TUPLE(t) -> if b then TUPLE t else TUPLE []
        | BOOL(b), STRING(s) -> if b then STRING s else STRING ""
        | _ -> EXCEPTION "Bool Error")) in
        
  (* createNone () should be createExp *)
  let floordiv =
    Map.upd mul "__floordiv__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | BOOL(b1), BOOL(b2) -> if not b1 then EXCEPTION "Bool Error" else if b2 then INT 1 else INT 0
        | BOOL(b1), INT(i) -> if not b1 then EXCEPTION "Bool Error" else INT (i/1)
        | _ -> EXCEPTION "Bool Error")) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | BOOL(b1), BOOL(b2) -> if not b1 then EXCEPTION "Bool Error" else INT 0
        | BOOL(b1), INT(i) -> if not b1 then EXCEPTION "Bool Error" else INT 0
        | _ -> EXCEPTION "Bool Error")) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | BOOL(b1), BOOL(b2) -> INT (if b1&&b2 then 2 else if b1||b2 then 1 else 0)
        | BOOL(b1), INT(i) -> INT (if b1 then 1+i else  i)
        | _ -> EXCEPTION "Bool Error")) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | BOOL(b1), BOOL(b2) -> INT (if b1&&b2 then 0 else if b1 then -1 else if b2 then 1 else 0)
        | BOOL(b1), INT(i) -> INT (if b1 then i-1 else  i)
        | _ -> EXCEPTION "Bool Error")) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | BOOL(b1), BOOL(b2) -> BOOL (if b1 then false else b2)
        | BOOL(b), INT(i) -> BOOL (if b then 1<i else 0<i)
        | _ -> EXCEPTION "Bool Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | BOOL(b1), BOOL(b2) -> BOOL (if b1 then b2 else true)
        | BOOL(b), INT(i) -> BOOL (if b then 1<=i else 0<=i)
        | _ -> EXCEPTION "Bool Error")) in

  let eq =
    Map.upd le "__eq__"
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with
        | BOOL(b1), BOOL(b2) -> BOOL (b1=b2)
        | BOOL(b), INT(i) -> BOOL (if b then i=1 else i=0)
        | _ -> EXCEPTION "Bool Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | INT(a), INT(b) -> BOOL (a <> b) 
        | INT(a), BOOL(b) -> BOOL (if b then a<>1 else a<>0)
        | _ -> EXCEPTION "Bool Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | BOOL(b1), BOOL(b2) -> BOOL (if b1 then not b2 else false)
        | BOOL(b), INT(i) -> BOOL (if b then 1>i else 0>i)
        | _ -> EXCEPTION "Bool Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | BOOL(b1), BOOL(b2) -> BOOL (if b1 then true else not b2)
        | BOOL(b), INT(i) -> BOOL (if b then 1>=i else 0>=i)
        | _ -> EXCEPTION "Bool Error")) in
  let allMethods = ge in
  let obj: cls= {
    name = "NoneType";
    pid = 0;
    value = BOOL(b);
    fields = emptyMap;
    methods = allMethods
  } in obj
