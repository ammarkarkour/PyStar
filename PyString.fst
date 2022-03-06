module PyString

open Structs
open Utils

let createString (s: string) =

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
                TUPLE([OBJ bobj; b])
            
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
                  TUPLE([nextString; newListIter])))
            
            | _ -> NONE)) in
            
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
      (UNOBJFUN (fun a -> createStringIter a)) in
      
  let mul =
    Map.upd emptyMap "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s), BOOL(b) -> if b then STRING(s) else STRING("")
        | STRING(s), INT(a) ->
          if 0 >= a
          then STRING("")
          else
          STRING(String.concat "" (tabulate (fun x -> s) a))
        | _ -> NONE)) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s1), STRING(s2) -> STRING(s2 ^  s1)
        | _ -> NONE)) in

  let lt =
    Map.upd add "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(false)
          | _ -> BOOL(true)) 
        | _ -> NONE)) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(false)
          | _ -> BOOL(true))
        | _ -> NONE)) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(false)
          | _ -> BOOL(false))
        | _ -> NONE)) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(true)
          | _ -> BOOL(true))
        | _ -> NONE)) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(false)
          | 1 -> BOOL(true)
          | _ -> BOOL(false))
        | _ -> NONE)) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> BOOL(true)
          | 1 -> BOOL(true)
          | _ -> BOOL(false))
        | _ -> NONE)) in
  let allMethods = ge in
  let obj: type0 = {
    name = "str";
    pid = 0;
    value = STRING s;
    fields = emptyMap;
    methods = allMethods
  } in
  PYTYP(OBJ(obj))
