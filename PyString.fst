module PyString

open Structs
open Utils

let rec createString (s: string) =

  let createStringIter a =
    match a with
    | OBJ lobj ->
      (* returns a tuple, (next element, new list_iterator) *)
      let next = 
        Map.upd (lobj.methods) "__next__"
          (UNFUN (fun b -> 
            match pyTypTobuiltins b with
            | STRING(s) ->
              (match String.list_of_string s with
              | [] -> 
                (* bobj is wrong, it should be replaced by Exception oncce py* support them *)
                let bobj = {
                  name = "StopIteration"; 
                  pid = 0;
                  value = STRING("");
                  fields = emptyMap;
                  methods = emptyMap
                } in
                createTuple ([OBJ bobj; b])
            
              | x::l -> 
                (match b with
                | OBJ bobj ->
                  let newListIter = OBJ({
                    name = "str_iterator";
                    pid = 0;
                    value = STRING(String.string_of_list l);
                    fields = bobj.fields;
                    methods = bobj.methods
                  }) in
                  let nextString = OBJ({
                    name = "str";
                    pid = 0;
                    value = STRING(String.make 1 x);
                    fields = bobj.fields;
                    methods = Map.upd  (bobj.methods) "__next__" (ERR "UNDEFINED") 
                  }) in
                  createTuple ([nextString; newListIter])))
            
            | _ -> createNone)) in
            
      let obj: type0= {
        name = "str_iterator";
        pid = 0;
        value = lobj.value;
        fields = lobj.fields;
        methods = next
      } in
      OBJ(obj) in
    
  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUN (fun a -> createStringIter a)) in
      
  let mul =
    Map.upd emptyMap "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s), BOOL(b) -> if b then createString s else createString ""
        | STRING(s), INT(a) ->
          if 0 >= a
          then createString ""
          else
          createString (String.concat "" (tabulate (fun x -> s) a))
        | _ -> createNone)) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s1), STRING(s2) -> createString (s2 ^  s1)
        | _ -> createNone)) in

  let lt =
    Map.upd add "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool false
          | _ -> createBool true) 
        | _ -> createNone)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool false
          | _ -> createBool true)
        | _ -> createNone)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool false
          | _ -> createBool false)
        | _ -> createNone)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool true
          | _ -> createBool true)
        | _ -> createNone)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool true
          | _ -> createBool false)
        | _ -> createNone)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool true
          | _ -> createBool false)
        | _ -> createNone)) in
  let allMethods = ge in
  let obj: type0 = {
    name = "str";
    pid = 0;
    value = STRING s;
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)
