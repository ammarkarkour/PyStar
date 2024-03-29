module PyList

open Structs
open Utils
open PyException

let createList (l: list cls) =
  
  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUNOBJ (fun a ->
        (* returns a tuple, (next element, new list_iterator) *)
        let next = 
          Map.upd (a.methods) "__next__"
          (UNFUNBLT (fun b -> 
            match b.value with
            | LIST([]) -> EXCEPTION "StopIteration"
            | LIST(x::l) ->
              let newListIter = {
                name = "list_iterator";
                pid = 0;
                value = LIST(l);
                fields = b.fields;
                methods = b.methods
              } in TUPLE ([x; newListIter])
            | _ -> EXCEPTION "List_Iterator Error")) in

        let obj: cls = {
          name = "list_iterator";
          pid = a.pid;
          value = a.value;
          fields = a.fields;
          methods = next
        } in obj)) in
        
  let mul =
    Map.upd iter "__mul__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l), BOOL(b) -> if b then LIST l else LIST []
        | LIST(l), INT(a) -> 
          if 0 >= a
          then LIST []
          else
          LIST (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> EXCEPTION "List Error")) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        |  LIST(l1), LIST(l2) -> LIST (List.append l1 l2)
        | _ -> EXCEPTION "List Error")) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b) with 
        | LIST(l1), obj -> BOOL (list_contains l1 obj) 
        | _ -> EXCEPTION "List Error")) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_lt l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_le l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_eq l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ne l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_gt l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ge l1 l2 with
          | BOOL(b) -> BOOL b
          | _ -> EXCEPTION "List Error")
        | _ -> EXCEPTION "List Error")) in

  let subscr =
      Map.upd ge "__subscr__"
        (BINFUNBLT (fun (a, b) ->
          match (a.value, b.value) with
          | LIST(l), INT(i) ->
            let l_len = List.length l in
            (match i >= 0 with
            | false ->
              (let new_i = l_len + i in
              match new_i < l_len with
              | true ->
                (match nth_int l new_i with
                | None -> EXCEPTION "List error"
                | Some c -> c.value)
              | false -> EXCEPTION "List error")
            
            | true  ->
              (match i < l_len with
              | true -> 
                (match nth_int l i with
                | None -> EXCEPTION "List error"
                | Some c -> c.value)
              
              | false -> EXCEPTION "List Error"))
          
          | LIST(l), SLICE start stop step ->
            let step =
              (match step with
              | None -> 1
              | Some s -> s) in
            (match step with
            | 0 -> EXCEPTION "List Error" (*S1*)
            | _ ->
              (match start <> None && start = stop with
              | true -> LIST [] (*S2*)
              | false ->
                let l_len = List.length l in
                (* set the lower bound *)
                let start =
                  (match start with
                  | None -> if step < 0 then l_len - 1 else 0
                  | Some s -> s) in
                (* set the upper bound *)
                let stop =
                  (match stop with
                  | None -> if step < 0 then -1 else l_len 
                  | Some s -> s) in
               LIST( get_slice l start stop step)))
          | _ -> EXCEPTION "List Error")) in

  let allMethods = subscr in
  let obj: cls = {
    name = "list";
    pid = 0;
    value = LIST(l);
    fields = emptyMap;
    methods = allMethods
  } in obj
