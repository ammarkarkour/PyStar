module PyTuple

open Structs
open Utils

let createTuple (l: list cls) = 

  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUNOBJ (fun a ->
        (* returns a tuple, (next element, new list_iterator) *)
        let next = 
          Map.upd (a.methods) "__next__"
          (UNFUNBLT (fun b -> 
            match b.value with
            | TUPLE([]) -> EXCEPTION "StopIteration"
            | TUPLE(x::l) ->
              let newListIter = {
                name = "tuple_iterator";
                pid = 0;
                value = TUPLE(l);
                fields = b.fields;
                methods = b.methods
              } in TUPLE ([x; newListIter])
            | _ -> EXCEPTION "Tuple_Iterator Error")) in

        let obj: cls = {
          name = "tuple_iterator";
          pid = a.pid;
          value = a.value;
          fields = a.fields;
          methods = next
        } in obj)) in
       
  let mul =
    Map.upd iter "__mul__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l), BOOL(b) -> if b then TUPLE l else TUPLE []
        | TUPLE(l), INT(a) -> 
          if 0 >= a
          then TUPLE []
          else
          TUPLE (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> EXCEPTION "Tuple Error")) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) -> TUPLE (List.append l1 l2)
        | _ -> EXCEPTION "Tuple Error")) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b) with 
        | TUPLE(l1), obj -> BOOL (list_contains l1 obj) 
        | _ -> EXCEPTION "Tuple Error")) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_lt l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error") 
        | _ -> EXCEPTION "Tuple Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_le l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error")
        | _ -> EXCEPTION "Tuple Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_eq l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error")
        | _ -> EXCEPTION "Tuple Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_ne l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error")
        | _ -> EXCEPTION "Tuple Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_gt l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error")
        | _ -> EXCEPTION "Tuple Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | TUPLE(l1), TUPLE(l2) -> 
          (match list_lex_ge l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "Tuple Error")
        | _ -> EXCEPTION "Tuple Error")) in
        
  let allMethods = ge in
  let obj: cls = {
    name = "tuple";
    pid = 0;
    value = TUPLE(l);
    fields = emptyMap;
    methods = add
  } in obj
