module PyList

open Structs
open Utils

let createList (l: list pyTyp) =
  let iter =
    Map.upd emptyMap "__iter__" 
      (UNOBJFUN (fun a -> 
         match a with
         | OBJ lobj ->
         (* returns a tuple, (next element, new list_iterator) *)
         let next = 
           Map.upd (lobj.methods) "__next__"
           (UNFUN (fun b -> 
             match pyTypTobuiltins b with
             | LIST([]) -> 
               (* bobj is wrong, it should be replaced by Exception oncce py* support them *)
               let bobj = {
                 name = "StopIteration"; 
                 pid = 0;
                 value = LIST([]);
                 fields = emptyMap;
                 methods = emptyMap
               } in
               createTuple ([OBJ bobj; b])
             | LIST(x::l) ->
               (match b with
               | OBJ bobj ->
                 let newListIter = OBJ({
                   name = "list_iterator";
                   pid = 0;
                   value = LIST(l);
                   fields = bobj.fields;
                   methods = bobj.methods
                 }) in
                 createTuple ([x; newListIter]))
             | _ -> createNone)) in
         let obj: type0= {
           name = "list_iterator";
           pid = 0;
           value = lobj.value;
           fields = lobj.fields;
           methods = next
         } in
         OBJ(obj))) in
        
  let mul =
    Map.upd iter "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l), BOOL(b) -> if b then createList l else createList []
        | LIST(l), INT(a) -> 
          if 0 >= a
          then createList []
          else
          LIST(List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> createNone)) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        |  LIST(l1), LIST(l2) -> createList (List.append l1 l2)
        | _ -> createNone)) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, b) with 
        | LIST(l1), obj -> createBool (list_contains l1 obj) 
        | _ -> createNone)) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_lt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_le l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_eq l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ne l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_gt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ge l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone)
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0= {
    name = "list";
    pid = 0;
    value = LIST(l);
    fields = emptyMap;
    methods = add
  } in
  OBJ(obj)
