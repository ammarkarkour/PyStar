module PyIntBool

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
        | _ -> createNone ())) in

  let neg =
    Map.upd pos "__neg__"
      (UNFUN (fun a ->
        match pyTypTobuiltins a with
        | INT(a) -> createInt (-a)
        | _ -> createNone ())) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | INT(a) ->  createBool (if a=0 then false else true)
        | _ -> createNone ())) in
        
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
        | _ -> createNone ())) in
  
  let floordiv =
    Map.upd mul "__floordiv__" 
            (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then createNone () else createInt (b/a) 
        | INT(a), BOOL(b) -> if a=0 then createNone () else if b then createInt (1/a) else createInt 0
        | _ -> createNone ())) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> if a=0 then createNone () else createInt (b%a)
        | INT(a), BOOL(b) -> if a=0 then createNone () else if b then createInt (1%a) else createInt 0
        | _ -> createNone ())) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (a + b) 
        | INT(a), BOOL(b) -> createInt (if b then 1+a else a)
        | _ -> createNone ())) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createInt (b - a) 
        | INT(a), BOOL(b) -> createInt (if b then 1+a else a)
        | _ -> createNone ())) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a < b) 
        | INT(a), BOOL(b) -> createBool (if b then a<1 else a<0)
        | _ -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a <= b) 
        | INT(a), BOOL(b) -> createBool (if b then a<=1 else a<=0)
        | _ -> createNone ())) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | INT(a), INT(b) -> createBool (a = b) 
        | INT(a), BOOL(b) -> createBool (if b then a=1 else a=0)
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a <> b) 
        | INT(a), BOOL(b) -> createBool (if b then a<>1 else a<>0)
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a > b) 
        | INT(a), BOOL(b) -> createBool (if b then a>1 else a>0)
        | _ -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | INT(a), INT(b) -> createBool (a >= b) 
        | INT(a), BOOL(b) -> createBool (if b then a>=1 else a>=0)
        | _ -> createNone ())) in
  let allMethods = ge in
  let obj: type0= {
    name = "int";
    pid = 0;
    value = INT(v);
    fields = emptyMap;
    methods = allMethods;
  } in
  OBJ(obj)

and createBool b =

  let pos = 
    Map.upd emptyMap "__pos__" 
      (UNFUN (fun a -> 
        match pyTypTobuiltins a with 
        | BOOL(b) -> createInt (if b then 1 else 0)
        | _ -> createNone ())) in

  let neg =
    Map.upd pos "__neg__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | BOOL b -> createInt (if b then -1 else 0)
        | _ -> createNone ())) in

  let bool =
    Map.upd neg "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | BOOL(b) ->  createBool b
        | _ -> createNone ())) in
        
  let mul =
    Map.upd bool "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 1 else 0)
        | BOOL(b), INT(i) -> createInt (if b then i else  0)
        | BOOL(b), LIST(l) -> if b then createList l else createList []
        | BOOL(b), TUPLE(t) -> if b then createTuple t else createTuple []
        | BOOL(b), STRING(s) -> if b then createString s else createString ""
        | _ -> createNone ())) in
        
  (* createNone () should be createExp *)
  let floordiv =
    Map.upd mul "__floordiv__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> if not b1 then createNone () else if b2 then createInt 1 else createInt 0
        | BOOL(b1), INT(i) -> if not b1 then createNone () else createInt (i/1)
        | _ -> createNone ())) in
  
  let modulo =
    Map.upd floordiv "__mod__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> if not b1 then createNone () else createInt 0
        | BOOL(b1), INT(i) -> if not b1 then createNone () else createInt 0
        | _ -> createNone ())) in
   
  let add =
    Map.upd modulo "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 2 else if b1||b2 then 1 else 0)
        | BOOL(b1), INT(i) -> createInt (if b1 then 1+i else  i)
        | _ -> createNone ())) in
        
  let sub = 
    Map.upd add "__sub__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createInt (if b1&&b2 then 0 else if b1 then -1 else if b2 then 1 else 0)
        | BOOL(b1), INT(i) -> createInt (if b1 then i-1 else  i)
        | _ -> createNone ())) in

  let lt =
    Map.upd sub "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then false else b2)
        | BOOL(b), INT(i) -> createBool (if b then 1<i else 0<i)
        | _ -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then b2 else true)
        | BOOL(b), INT(i) -> createBool (if b then 1<=i else 0<=i)
        | _ -> createNone ())) in

  let eq =
    Map.upd le "__eq__"
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (b1=b2)
        | BOOL(b), INT(i) -> createBool (if b then i=1 else i=0)
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | INT(a), INT(b) -> createBool (a <> b) 
        | INT(a), BOOL(b) -> createBool (if b then a<>1 else a<>0)
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then not b2 else false)
        | BOOL(b), INT(i) -> createBool (if b then 1>i else 0>i)
        | _ -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | BOOL(b1), BOOL(b2) -> createBool (if b1 then true else not b2)
        | BOOL(b), INT(i) -> createBool (if b then 1>=i else 0>=i)
        | _ -> createNone ())) in
  let allMethods = ge in
  let obj: type0= {
    name = "NoneType";
    pid = 0;
    value = BOOL(b);
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)

and createNone () = 

  let bool =
    Map.upd emptyMap "__bool__" 
      (UNFUN (fun a ->
        match pyTypTobuiltins a with 
        | NONE -> createBool false
        | _ -> createNone ())) in

  (* Should return NotImplemented (Something similar to None) *)
  let lt =
    Map.upd bool "__lt__" 
      (BINFUN (fun (a, b) -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->  createNone ())) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> createBool true 
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | NONE, NONE -> createBool false
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) ->  createNone ())) in
  let allMethods = ge in
  let obj: type0 = {
    name = "NoneType";
    pid = 0;
    value = NONE;
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)

and createString (s: string) =

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
            
            | _ -> createNone ())) in
            
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
        | _ -> createNone ())) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(s1), STRING(s2) -> createString (s2 ^  s1)
        | _ -> createNone ())) in

  let lt =
    Map.upd add "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool false
          | _ -> createBool true) 
        | _ -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool false
          | _ -> createBool true)
        | _ -> createNone ())) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool false
          | _ -> createBool false)
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool true
          | _ -> createBool true)
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool false
          | 1 -> createBool true
          | _ -> createBool false)
        | _ -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with
        | STRING(a), STRING(b) -> 
          (match String.compare a b with
          | 0 -> createBool true
          | 1 -> createBool true
          | _ -> createBool false)
        | _ -> createNone ())) in
  let allMethods = ge in
  let obj: type0 = {
    name = "str";
    pid = 0;
    value = STRING s;
    fields = emptyMap;
    methods = allMethods
  } in
  OBJ(obj)

(* was not recursive *)
and createList (l: list pyTyp) =

  let dummy = UNFUN (fun a -> createList []) in
  
  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUN (fun a -> 
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
             | _ -> createNone ())) in
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
          createList (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> createNone ())) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        |  LIST(l1), LIST(l2) -> createList (List.append l1 l2)
        | _ -> createNone ())) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, b) with 
        | LIST(l1), obj -> createBool (list_contains l1 obj) 
        | _ -> createNone ())) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_lt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_le l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_eq l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ne l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_gt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | LIST(l1), LIST(l2) ->
          (match list_lex_ge l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in
  let allMethods = ge in
  let obj: type0= {
    name = "list";
    pid = 0;
    value = LIST(l);
    fields = emptyMap;
    methods = add
  } in
  OBJ(obj)
  
and createTuple (l: list pyTyp) = 

  let iter =
    Map.upd emptyMap "__iter__" 
      (UNFUN (fun a ->
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
                (match x with
                | OBJ xobj ->
                  let newListIter = OBJ({
                    name = "tuple_iterator";
                    pid = xobj.pid;
                    value = LIST(l);
                    fields = xobj.fields;
                    methods = xobj.methods
                  }) in
                  createTuple ([x; newListIter]))
              | _ -> createNone ())) in
        
        let obj: type0= {
          name = "tuple_iterator";
          pid = lobj.pid;
          value = lobj.value;
          fields = lobj.fields;
          methods = next
        } in
        OBJ(obj))) in
      
  let mul =
    Map.upd iter "__mul__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l), BOOL(b) -> if b then createTuple l else createTuple []
        | TUPLE(l), INT(a) -> 
          if 0 >= a
          then createTuple []
          else
          createTuple (List.Tot.Base.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) a))
        | _ -> createNone ())) in
   
  let add =
    Map.upd mul "__add__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> createTuple (List.append l1 l2)
        | _ -> createNone ())) in

  let contains =
    Map.upd add "__contains__" 
      (BINFUN (fun (a, b) ->
        match (pyTypTobuiltins a, b) with 
        | TUPLE(l1), obj -> createBool (list_contains l1 obj) 
        | _ -> createNone ())) in
        
  let lt =
    Map.upd contains "__lt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_lt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ()) 
        | _ -> createNone ())) in
        
  let le =
    Map.upd lt "__le__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_le l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let eq =
    Map.upd le "__eq__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_eq l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let neq =
    Map.upd eq "__ne__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_ne l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let gt =
    Map.upd neq "__gt__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) ->
          (match list_lex_gt l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in

  let ge =
    Map.upd gt "__ge__" 
      (BINFUN (fun (a, b) -> 
        match (pyTypTobuiltins a, pyTypTobuiltins b) with 
        | TUPLE(l1), TUPLE(l2) -> 
          (match list_lex_ge l1 l2 with
          | BOOL(b) -> createBool b
          | _ -> createNone ())
        | _ -> createNone ())) in
  let allMethods = ge in
  let obj: type0= {
    name = "tuple";
    pid = 0;
    value = TUPLE(l);
    fields = emptyMap;
    methods = add
  } in
  OBJ(obj)

