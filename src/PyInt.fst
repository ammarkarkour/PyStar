module PyInt

(* imported modules *)
open Structs
open Utils
(* ---------------- *)

(* Functions used to create objects of builtin types *)
let createInt (v:int) =

  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUNBLT (fun a -> 
        match a.value with 
        | INT(x) -> INT x
        | _ -> EXCEPTION "Int Error")) in

  let neg =
    Map.upd pos "__neg__"
      (UNFUNBLT (fun a ->
        match a.value with
        | INT(a) -> INT (-a)
        | _ -> EXCEPTION "Int Error")) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUNBLT (fun a ->
        match a.value with 
        | INT(a) ->  BOOL (if a=0 then false else true)
        | _ -> EXCEPTION "Int Error")) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> INT (op_Multiply b a) 
        | INT(a), BOOL(b) -> INT (if b then a else 0)
        | INT(a), STRING(s) ->
          if 0 >= a
          then STRING ""
          else
          STRING (String.concat "" (tabulate (fun x -> s) a))
        | INT(a), LIST(l) -> 
          if 0 >= a
          then LIST []
          else
          LIST (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | INT(a), TUPLE(l) ->
          if 0 >= a
          then TUPLE []
          else
          TUPLE (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> EXCEPTION "Int Error")) in
  
  let floordiv =
    Map.upd mul "__floordiv__" 
            (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> if a=0 then EXCEPTION "Int Error" else INT (b/a) 
        | INT(a), BOOL(b) -> if a=0 then EXCEPTION "Int Error" else if b then INT (1/a) else INT 0
        | _ -> EXCEPTION "Int Error")) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> if a=0 then EXCEPTION "Int Error" else INT (b%a)
        | INT(a), BOOL(b) -> if a=0 then EXCEPTION "Int Error" else if b then INT (1%a) else INT 0
        | _ -> EXCEPTION "Int Error")) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> INT (a + b) 
        | INT(a), BOOL(b) -> INT (if b then 1+a else a)
        | _ -> EXCEPTION "Int Error")) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> INT (a - b) 
        | INT(a), BOOL(b) -> INT (if b then a-1 else a)
        | _ -> EXCEPTION "Int Error")) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with 
        | INT(a), INT(b) -> BOOL (a < b) 
        | INT(a), BOOL(b) -> BOOL (if b then a<1 else a<0)
        | _ -> EXCEPTION "Int Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with 
        | INT(a), INT(b) -> BOOL (a <= b) 
        | INT(a), BOOL(b) -> BOOL (if b then a<=1 else a<=0)
        | _ -> EXCEPTION "Int Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | INT(a), INT(b) -> BOOL (a = b) 
        | INT(a), BOOL(b) -> BOOL (if b then a=1 else a=0)
        | _ -> EXCEPTION "Int Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> BOOL (a <> b) 
        | INT(a), BOOL(b) -> BOOL (if b then a<>1 else a<>0)
        | _ -> EXCEPTION "Int Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with 
        | INT(a), INT(b) -> BOOL (a > b) 
        | INT(a), BOOL(b) -> BOOL (if b then a>1 else a>0)
        | _ -> EXCEPTION "Int Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | INT(a), INT(b) -> BOOL (a >= b) 
        | INT(a), BOOL(b) -> BOOL (if b then a>=1 else a>=0)
        | _ -> EXCEPTION "Int Error")) in
  let allMethods = ge in
  let obj: cls = {
    name = "int";
    pid = 0;
    value = INT(v);
    fields = emptyMap;
    methods = allMethods;
  } in obj

