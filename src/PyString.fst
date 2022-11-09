module PyString

open Structs
open Utils

let createString (s: string) =
    
  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUNOBJ (fun a ->
        (* returns a tuple, (next element, new list_iterator) *)
        let next = 
          Map.upd (a.methods) "__next__"
            (UNFUNBLT (fun b -> 
              match b.value with
              | STRING(s) ->
                (match String.list_of_string s with
                | [] -> EXCEPTION "StopIteration"
                | x::l -> 
                  let newListIter = {
                    name = "str_iterator";
                    pid = 0;
                    value = STRING(String.string_of_list l);
                    fields = b.fields;
                    methods = b.methods
                  } in
                  let nextString = {
                    name = "str_iterator";
                    pid = 0;
                    value = STRING(String.make 1 x);
                    fields = b.fields;
                    methods = b.methods
                  } in TUPLE ([nextString; newListIter]))
              | _ -> EXCEPTION "Str_Iterator Error")) in

        let obj: cls = {
          name = "str_iterator";
          pid = a.pid;
          value = a.value;
          fields = a.fields;
          methods = next
        } in obj)) in
      
  let mul =
    Map.upd iter "__mul__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | STRING(s), BOOL(b) -> if b then STRING s else STRING ""
        | STRING(s), INT(a) ->
          if 0 >= a
          then STRING ""
          else
          STRING (String.concat "" (tabulate (fun x -> s) a))
        | _ -> EXCEPTION "String Error")) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with
        | STRING(s1), STRING(s2) -> STRING (s2 ^  s1)
        | _ -> EXCEPTION "String Error")) in

  let lt =
    Map.upd add "__lt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL false
          | 1 -> BOOL false
          | _ -> BOOL true) 
        | _ -> EXCEPTION "String Error")) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL true
          | 1 -> BOOL false
          | _ -> BOOL true)
        | _ -> EXCEPTION "String Error")) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL true
          | 1 -> BOOL false
          | _ -> BOOL false)
        | _ -> EXCEPTION "String Error")) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUNBLT (fun (a, b) ->
        match (a.value, b.value) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL false
          | 1 -> BOOL true
          | _ -> BOOL true)
        | _ -> EXCEPTION "String Error")) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL false
          | 1 -> BOOL true
          | _ -> BOOL false)
        | _ -> EXCEPTION "String Error")) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUNBLT (fun (a, b) -> 
        match (a.value, b.value) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL true
          | 1 -> BOOL true
          | _ -> BOOL false)
        | _ -> EXCEPTION "String Error")) in

  let subscr =
      Map.upd ge "__subscr__"
        (BINFUNBLT (fun (a, b) ->
          match (a.value, b.value) with
          | STRING(s), INT(i) ->
            (match subString_pos s i with
            | Some c -> STRING c
            | None -> EXCEPTION "String Error")
          | _ -> EXCEPTION "String Error")) in
          
  let allMethods = subscr in
  let obj: cls = {
    name = "str";
    pid = 0;
    value = STRING s;
    fields = emptyMap;
    methods = allMethods
  } in obj
