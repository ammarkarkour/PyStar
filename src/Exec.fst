module Exec

(*
Notaion: 
  - tos means top of stack (i.e., datastack[0])
    - for the second elemnt in the stack and the elements following it:
      - The (i+1)th element is represtned as tosi
      - eg: 2nd element = tos1, 3rd element = tos2, ...
  
  - BOOL(b): b in F* is true or false, but in python evaluation it'streated as int
    - True = 1 and False = 0
    - So in req and Ens b in BOOL(b) is just 1 or 0

  - res is what a function evaluates to. 
*) 

 let builtinsToPyObj (bltin: builtins) = 
   match bltin with
   | INT n -> createInt n
   | STRING s -> createString s
   | BOOL b -> createBool b
   | LIST l -> createList l
   | TUPLE t -> createTuple t
   | DICT kvl -> createDict kvl 
   | FUNCTION f -> createFunction f
   | EXCEPTION s -> createException s
   | NONE -> createNone ()
   | USERDEF -> createException "Creating_userdefined Error"
  
(* Create a new frame and push it in the frame stack *)
let makeFrame virM code localplus global_names local_names =
  let frame: frameObj = {
    dataStack = []; 
    blockStack = [];
    fCode = code;
    f_localplus = localplus;
    pc = 0;
    f_globals = global_names;
    f_locals = local_names;
    f_idCount = virM.idCount;
    f_usedIds = virM.usedIds
  } in
  (virM, frame)

(*
  Req:
  Ens:
*)
let call_function i dataStack id usedIds =
  let args, newDataStack = splitAt i dataStack in
  let localplus = rev args in
  let code, restStack = splitAt 1 newDataStack in
  match nth code 0 with
  | None -> (undefinedBehavior "call_function_1")::dataStack
  | Some (PYTYP obj) -> 
    (match obj.value with
     | FUNCTION func -> 
       (match func.func_Code with
        | CODEOBJECT co ->
          (let newFrame: frameObj = {
            dataStack = [];
            blockStack = [];
            fCode = co;
            f_localplus = localplus;
            pc = 0;
            f_globals = func.func_globals;
            f_locals = emptyMap;
            f_idCount = id;
            f_usedIds = usedIds
          } in (FRAMEOBJECT newFrame)::restStack)
        | _ -> (undefinedBehavior "call_function_2")::dataStack)
     | _ -> (undefinedBehavior "call_function_3")::dataStack)
  | _ -> (undefinedBehavior "call_function_4")::dataStack

(*
   Req: length(datastack) >= 1
   Ens: res = datastack[1:]
*)
let pop_top datastack = tail datastack

let pop_top_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 1)
          (ensures  (length datastack)-1 = length (pop_top datastack))
  = ()
  
(*
   Req: length(datastack) >= 2
   Ens: res = tos1::tos::datastack[2:]
*)
let rot_two datastack = 
  let tos = hd datastack in
  let tos1 = nth datastack 1 in
  match tos1 with
  | None -> (undefinedBehavior "rot_two")::datastack
  | Some tos1 ->
    let _, rest_stack = splitAt 2 datastack in
    tos1::tos::rest_stack

let rot_two_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 2)
          (ensures length (rot_two datastack) = length datastack)
  = ()
  
(*
   Req: length(datastack) >= 3
   Ens: res = tos1::tos2::tos::datastack[3:]
*)
let rot_three datastack = 
  let tos = hd datastack in
  let tos1 = nth datastack 1 in
  match tos1 with
  | None -> (undefinedBehavior "rot_three_1")::datastack
  | Some tos1 ->
    let tos2 = nth datastack 2 in
    (match tos2 with
    | None -> (undefinedBehavior "rot_three_2")::datastack
    | Some tos2 ->
      let _, rest_stack = splitAt 3 datastack in
      tos1::tos2::tos::rest_stack)

let rot_three_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 3)
          (ensures length (rot_two datastack) = length datastack)
  = ()
  
(*
   Req: length(datastack) >= 4
   Ens: res = tos1::tos2::tos3::tos::datastack[4:]
*)
let rot_four datastack = 
  let tos = hd datastack in
  let tos1 = nth datastack 1 in
  match tos1 with
  | None -> (undefinedBehavior "rot_four_1")::datastack
  | Some tos1 ->
    let tos2 = nth datastack 2 in
    (match tos2 with
    | None -> (undefinedBehavior "rot_four_2")::datastack
    | Some tos2 ->
      let tos3 = nth datastack 3 in
      (match tos3 with
      | None -> (undefinedBehavior "rot_four_3")::datastack
      | Some tos3 ->
        let _, rest_stack = splitAt 4 datastack in
        tos1::tos2::tos3::tos::rest_stack))

let rot_four_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 4)
          (ensures length (rot_two datastack) = length datastack)
  = ()

(*
   Req: length(datastack) >= 1
   Ens: res = tos::datastack
*)
let dup_top datastack = (hd datastack)::datastack

let dup_top_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 1)
          (ensures (length datastack)+1 = length (dup_top datastack))
  = ()
  
(*
   Req: length(datastack) >= 2
   Ens: res = tos::tos1::datastack
*)
let dup_top_two datastack = 
  let tos = hd datastack in
  let tos1 = nth datastack 1 in
  match tos1 with
  | None -> (undefinedBehavior "dup_top")::datastack
  | Some tos1 ->  tos::tos1::datastack

let dup_top_two_len_prop (datastack: list pyObj)
  : Lemma (requires length datastack >= 2)
          (ensures (length datastack)+2 = length (dup_top_two datastack))
  = ()

(*
   Req: length(datastack) >= 1
   Ens: res = +tos::datastack[1:]
*)
let unary_positive datastack = 
  let tos = hd datastack in
  let newDataStack = tail datastack in
  match tos with
  | PYTYP(obj) -> 
    (match (Map.sel (obj.methods) "__pos__") with
    | UNFUNBLT f -> PYTYP(builtinsToPyObj (f obj) )::newDataStack
    | ERR s -> PYTYP(createException "__pos__ is not defined" )::newDataStack
    | _ -> (undefinedBehavior "unary_positive_1")::newDataStack)
  | _ -> (undefinedBehavior "unary_positive_2")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens: res = -tos::datastack[1:]
*)
let unary_negative datastack  = 
  let tos = hd datastack in
  let newDataStack = tail datastack in
  match tos with
  | PYTYP(obj) -> 
    (match (Map.sel (obj.methods) "__neg__") with
    | UNFUNBLT f -> PYTYP(builtinsToPyObj (f obj) )::newDataStack
    | ERR s -> PYTYP(createException "__neg__ is not defined" )::newDataStack
    | _ -> (undefinedBehavior "unary_negative_1")::newDataStack)
  | _ -> (undefinedBehavior "unary_negative_2")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens: res = (not tos)::datastack[1:]
*)
let unary_not datastack  =
  let tos = hd datastack in
  let newDataStack = tail datastack in
  match tos with
  | PYTYP(obj) ->
    let res = 
      (match obj.value with
      | INT i -> if i=0 then true else false
      | STRING s -> if s="" then true else false
      | BOOL b -> if b then false else true
      | LIST l -> (match l with [] -> true | _ -> false)
      | TUPLE t -> (match t with [] -> true | _ -> false)
      | DICT kvl -> (match kvl with [] -> true | _ -> false)
      | FUNCTION f -> false
      | EXCEPTION s -> false
      | USERDEF -> false
      | NONE -> true) in PYTYP(createBool res )::newDataStack
  | _ -> (undefinedBehavior "unary_not")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens: res = iter(tos)::datastack[2:]
*)
let get_iter datastack  = 
  let tos = hd datastack in
  let newDataStack = tail datastack in
  match tos with
  | PYTYP(obj) -> 
    (match (Map.sel (obj.methods) "__iter__") with
    | UNFUNOBJ f -> PYTYP(f obj)::newDataStack
    | ERR s -> PYTYP(createException "__iter__ is not defined" )::newDataStack
    | _ -> (undefinedBehavior "get_iter_1")::newDataStack)
  | _ -> (undefinedBehavior "get_iter_2")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res = (tos1*tos)::datastack[2:]
*)
let binary_multiply dataStack  = 
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "binary_multiply_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in 
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__mul__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__mul__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_multiply_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_multiply_3")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res = (tos1//tos)::datastack[2:]
*)
let binary_floor_divide dataStack  = 
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "binary_floor_divide_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in 
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__floordiv__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__floordiv__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_floor_divide_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_floor_divide_3")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res = (tos1%tos)::datastack[2:]
*)
let binary_modulo dataStack =
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "bianry_modulo_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in 
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__mod__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__mod__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_modulo_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_modulo_3")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res =(tos1+tos)::datastack[2:]
*)
let binary_add dataStack = 
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "binary_add_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__add__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__add__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_add_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_add_3")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res = (tos1-tos)::datastack[2:]
*)
let binary_subtract dataStack = 
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "binary_subtract_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in 
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__sub__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__sub__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_subtract_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_subtract_3")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: res = tos1[tos]::datastack[2:]
*)
let binary_subscr dataStack = 
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "binary_subscr_1")::dataStack
  | Some tos1 ->
    let (_, newDataStack) = splitAt 2 dataStack in 
    match (tos1, tos) with
    | PYTYP(obj1), PYTYP(obj2) -> 
      (match (Map.sel (obj1.methods) "__subscr__") with
      | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
      | ERR s -> PYTYP(createException "__subscr__ is not defined" )::newDataStack
      | _ -> (undefinedBehavior "binary_subscr_2")::newDataStack)
    | _, _ -> (undefinedBehavior "binary_subscr_3")::newDataStack

(*
  Req: (length(datastack) >= i)
  Ens: res = (tosi, ..., tos)::datastack[i+1:]
*)
let build_tuple i dataStack  =
  let elems, newDataStack  = splitAt i dataStack in
  match unwrapPyObjList elems with
  | None -> (undefinedBehavior "build_tuple")::dataStack
  | Some l -> PYTYP(createTuple (rev l))::newDataStack

(*
  Req: (length(datastack) >= i)
  Ens: res = [tos, ..., tosi]::datastack[i+1:]
*)
let build_list i dataStack  =
  let elems, newDataStack  = splitAt i dataStack in
  match unwrapPyObjList elems with
  | None -> (undefinedBehavior "build_list")::dataStack
  | Some l -> PYTYP(createList (rev l))::newDataStack

(*
  Req: (length(datastack) >= 2*conut)
  Ens: res = {..., TOS3: TOS2, TOS1: TOS}::datastack[2*count+1:]
*)
let build_map count dataStack  =
  let elems, newDataStack  = splitAt (op_Multiply 2 count) dataStack in
  match unwrapPyObjList elems with
  | None -> (undefinedBehavior "build_map_1")::dataStack
  | Some clsElems ->
    (match (length clsElems)%2 = 0 with
    | false -> (undefinedBehavior "build_map_2")::dataStack
    | true -> 
      let vkl = listToPairs clsElems in
      let newDataStack = PYTYP(createDict vkl)::newDataStack in
      newDataStack)

(*
  Req: (length(datastack) >= conut+1)
  Ens: res = DICT(tos, [tos1, .. tosi])::datastack[i+2:]
*)
let build_const_key_map count dataStack  =
  let keysObj, newDataStack = splitAt 1 dataStack in
  match keysObj with
  | [PYTYP obj] ->
    (match obj.value with
    | TUPLE keysCls ->
      (let vals, newDataStack  = splitAt count newDataStack in
       (match unwrapPyObjList vals with
       | None -> (undefinedBehavior "build_const_key_map_2")::newDataStack
       | Some valsCls ->
         (match length valsCls = length keysCls with
         | false -> (undefinedBehavior "build_const_key_map")::dataStack
         | true -> 
           let vkl = totZip valsCls keysCls in
           let newDataStack = PYTYP(createDict vkl)::newDataStack in
           newDataStack)))
    | _ -> (undefinedBehavior "build_const_key_map_2")::newDataStack)
  | _ -> (undefinedBehavior "build_const_key_map_3")::newDataStack
  
(*
  Req: length(datastack) >= 2
  Ens: res = (op tos1 tos)::datastack[2:]
*)
let compare_op i dataStack  =
  let tos = hd dataStack in
  let tos1 = nth dataStack 1 in
  match tos1 with
  | None -> (undefinedBehavior "compare_op_3")::dataStack
  | Some tos1 -> 
    let (_, newDataStack) = splitAt 2 dataStack in
    let op =
      match i with
      | 0 -> "__lt__"
      | 1 -> "__le__"
      | 2 -> "__eq__"
      | 3 -> "__ne__"
      | 4 -> "__gt__"
      | 5 -> "__ge__"
      | 6 -> "__contains__" (* in *)
      | 7 -> "__contains__" (* not in*)
      | 8 -> "__is__"
      | 9 -> "__nis__"
      | _ -> "error" in
    match op with
    | "__eq__" ->
      (match (tos1, tos) with
      | PYTYP(obj1), PYTYP(obj2) -> 
        (match (Map.sel (obj1.methods) op) with
        | BINFUNBLT f -> 
          (match f(obj1, obj2) with
            | BOOL b -> PYTYP(createBool b )::newDataStack
            | _ -> PYTYP(createBool (obj1.pid = obj2.pid) )::newDataStack)
        | _ -> (undefinedBehavior "compare_op_eq_1")::newDataStack)
      | _,_ -> (undefinedBehavior "compare_op_eq_2")::newDataStack)
    | "__ne__" ->
      (match (tos1, tos) with
      | PYTYP(obj1), PYTYP(obj2) -> 
        (match (Map.sel (obj1.methods) op) with
        | BINFUNBLT f -> 
          (match f(obj1, obj2) with
            | BOOL b -> PYTYP(createBool b )::newDataStack
            | _ -> PYTYP(createBool (obj1.pid <> obj2.pid) )::newDataStack)
        | _ -> (undefinedBehavior "compare_op_ne_1")::newDataStack)
      | _,_ -> (undefinedBehavior "compare_op_ne_2")::newDataStack)
    | "__is__" ->
      (match (tos1, tos) with
      | PYTYP(obj1), PYTYP(obj2) -> PYTYP(createBool (obj1.pid = obj2.pid) )::newDataStack
      | _ -> (undefinedBehavior "compare_op_is")::newDataStack)
    | "__nis__" ->
      (match (tos1, tos) with
      | PYTYP(obj1), PYTYP(obj2) -> PYTYP(createBool (obj1.pid <> obj2.pid) )::newDataStack
      | _ -> (undefinedBehavior "compare_op_nis")::newDataStack)
    | "error" -> (undefinedBehavior "compare_op_error")::newDataStack
    | _ ->
      match (tos1, tos) with
      | PYTYP(obj1), PYTYP(obj2) -> 
        (match (Map.sel (obj1.methods) op) with
        | BINFUNBLT f -> PYTYP(builtinsToPyObj (f(obj1, obj2)) )::newDataStack
        | ERR s -> PYTYP(createException "__pos__ is not defined" )::newDataStack
        | _ -> (undefinedBehavior "compare_op_2")::newDataStack)
      | _, _ -> (undefinedBehavior "compare_op_3")::newDataStack

(*
  Req: (length names) >= i && (length datastack) > 1 
  Ens: res = (f_locals with f_locals[name] = tos, datastack[1:])
*)
let store_name i names f_locals dataStack = 
  let tos = hd dataStack in
  let name = nth names i in
  match name with
  | None -> (f_locals, (undefinedBehavior "store_name")::dataStack)
  | Some name -> 
    let _, newDataStack = splitAt 1 dataStack in 
    let newLocals = Map.upd f_locals name tos in
    (newLocals, newDataStack)
  
  
(*
  Req: (length(consts) > i) && (length(datastack) >= 1)
  Ens: res = consts[i]::datastack
*)
let load_const i consts dataStack =
  let elem = nth consts i in
  match elem with
  | None -> (undefinedBehavior "load_constant")::dataStack
  | Some elem ->
    let newDataStack = elem::dataStack in
    newDataStack

(*
  Req: (length names) > i
  Ens: res = f_locals[names[i]]::datastack  ||
       res = f_globals[names[i]]::datastack ||
       res = exception::datastack
*)
let load_name i names f_locals f_globals dataStack  =
  let name = nth names i in
  match name with
  | None -> (undefinedBehavior "load_name")::dataStack
  | Some name ->
    (match Map.contains f_locals name with
    | true -> (Map.sel f_locals name)::dataStack 
    | false ->
      (match Map.contains f_globals name with
       | true -> (Map.sel f_globals name)::dataStack
       | false -> PYTYP(createException ("name: " ^ name ^ "is not defined"))::dataStack))

(*
  Req: 
  Ens:
*)
let pop_jump_if_true i pc dataStack  =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
  match tos with
  | PYTYP(obj) ->
    (match obj.value with
    | BOOL b -> if b then (i/2, newDataStack) else (pc, newDataStack)
    | _ -> (pc, PYTYP(createException "ERR: argument is not a Bool" )::newDataStack))
  | _ -> (pc, (undefinedBehavior "pop_jump_if_true")::newDataStack)

(*
  Req:
  Ens:
*)
let pop_jump_if_false i pc dataStack  =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
  match tos with
  | PYTYP(obj) ->
    (match obj.value with
    | BOOL b -> if b then (pc, newDataStack) else (i/2, newDataStack)
    | _ -> (pc, PYTYP(createException "ERR: argument is not a Bool" )::newDataStack))
  | _ -> (pc, (undefinedBehavior "pop_jump_if_false")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_true_or_pop i pc dataStack  =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
  match tos with
  | PYTYP(obj) ->
    (match obj.value with
    | BOOL b -> if b then (i/2, dataStack) else (pc, newDataStack)
    | _ -> (pc, PYTYP(createException "ERR: argument is not a Bool" )::newDataStack))
  | _ -> (pc, (undefinedBehavior "jump_if_true_or_pop")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_false_or_pop i pc dataStack  =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
  match tos with
  | PYTYP(obj) ->
    (match obj.value with
    | BOOL b -> if b then (pc, newDataStack) else (i/2, dataStack)
    | _ -> (pc, PYTYP(createException "ERR: argument is not a Bool" )::newDataStack))
  | _ -> (pc, (undefinedBehavior "jump_if_false_or_pop")::newDataStack)

(*
   Req:
   Ens:
*)
let for_iter i pc dataStack  =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
   match tos with
  | PYTYP(obj) -> 
    (match (Map.sel (obj.methods) "__next__") with
    | UNFUNBLT f -> 
      (match f obj with
      | TUPLE [x; newIter] -> (pc, PYTYP(x)::PYTYP(newIter)::newDataStack)
      | EXCEPTION "StopIteration" -> (pc+i, newDataStack)
      | _ -> (pc, (undefinedBehavior "for_iter_1")::newDataStack))
    |  ERR s -> (pc, PYTYP(createException "Not iteratable object" )::newDataStack)
    | _ -> (pc, (undefinedBehavior "for_iter_2")::newDataStack))
  | _ -> (pc, (undefinedBehavior "for_iter_3")::newDataStack)

(*
  Req:
  Ens:
*)
let load_global i names f_globals dataStack  =
  let name = nth names i in
  match name with
  | None -> (undefinedBehavior "load_global")::dataStack
  | Some name ->
    match Map.contains f_globals name with
    | true -> (Map.sel f_globals name)::dataStack
    | false -> PYTYP(createException ("name: " ^ name ^ "is not defined") )::dataStack

(*
  Req: (length(localplus) > i)
  Ens: res = localplus[i]::dataStack
*)
let load_fast i localPlus dataStack =
  let elem = nth localPlus i in
  match elem with
  | None -> (undefinedBehavior "load_fast")::dataStack
  | Some elem ->
    let newDataStack = elem::dataStack in
    newDataStack

(*
  Req: (length f_localplus) >= i 
  Ens: res = (localplus where localplus[i] = tos, datastack[1:])
*)
let store_fast i localplus dataStack =
  let tos = hd dataStack in
  let newDataStack = tail dataStack in
    match (length localplus) > i with
    | true ->
      let newLocalPlus = mapi (fun k x -> if k = i then tos else x) localplus in
      (newLocalPlus, newDataStack)
    | false ->
      let newLocalPlus = append localplus [tos] in
      (newLocalPlus, newDataStack) 

(*
  Req:
  Ens:
*)
let make_function flags globs dataStack =
  let qualname = hd dataStack in
  let codeobj = nth dataStack 1 in
  match codeobj with
  | None -> (undefinedBehavior "make_function_1")::dataStack
  | Some (CODEOBJECT co) ->
    let (_, newDataStack) = splitAt 2 dataStack in
    let func = createFunction ({
      func_Code = CODEOBJECT co;
      func_globals = globs;
      func_name = qualname;
      func_closure =
        if flags = 8 then
         (match hd newDataStack with
          | PYTYP(obj) ->
            (match obj.value with
             | TUPLE(t) -> PYTYP(createTuple t)
             | _ -> undefinedBehavior "make_function_func_closure_2")
          | _ -> undefinedBehavior "make_function_func_closure_3") else PYTYP(createNone());

      func_defaults =
        if flags = 1 then
         (match hd newDataStack with
          | PYTYP(obj) ->
            (match obj.value with
             | TUPLE(t) -> PYTYP(createTuple t)
             | _ -> undefinedBehavior "make_function_func_defaults_2")
          | _ -> undefinedBehavior "make_function_func_defaults_3") else PYTYP(createNone());
    }) in
    (match flags with
     | 0 -> PYTYP(func)::newDataStack
     | _ -> PYTYP(func)::(tail newDataStack))
  | _ -> (undefinedBehavior "make_function_3")::dataStack 

(*
   Req: len(frame.fcode.bytecode) >= 1
*)
let rec execBytecode frame  =
  match check_err frame.dataStack with
  | Some err -> frame
  | None -> 
  
    let CODE(bc) = frame.fCode.co_code in
    match List.nth bc (frame.pc) with
    | RETURN_VALUE -> frame
    
    | CALL_FUNCTION i ->
      (match length frame.dataStack > i with
      | false ->
        let newDataStack = [undefinedBehavior "CALL_FUNCTION"] in
          ({frame with dataStack = newDataStack})
      | true ->
        let newDataStack = call_function i frame.dataStack
                         frame.f_idCount frame.f_usedIds in
        ({frame with dataStack = newDataStack}))
    
    | NOP -> execBytecode ({frame with pc = frame.pc+1})
    
    | POP_TOP ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "POP_TOP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = pop_top frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | ROT_TWO ->
      (match (length frame.dataStack) >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "ROT_TWO"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = rot_two frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | ROT_THREE ->
      (match length frame.dataStack >= 3 with
      | false ->
        let newDataStack = [undefinedBehavior "ROT_THREE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = rot_three frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | ROT_FOUR ->
      (match length frame.dataStack >= 4 with
      | false ->
        let newDataStack = [undefinedBehavior "ROT_FOUR"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = rot_four frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | DUP_TOP ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "DUP_TOP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = dup_top frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | DUP_TOP_TWO ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "DUP_TOP_TWO"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = dup_top_two frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | UNARY_POSITIVE ->
      (match frame.dataStack with
      | [] -> 
        let newDataStack = [undefinedBehavior "UNARY_POSITIVE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = unary_positive frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | UNARY_NEGATIVE ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "UNARY_NEGATIVE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = unary_negative frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | UNARY_NOT ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "UNARY_NOT"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = unary_not frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | GET_ITER ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "GET_ITER"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newDataStack = get_iter frame.dataStack in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_MULTIPLY ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_MULTIPLY"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_multiply (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_FLOOR_DIVIDE ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_FLOOR_DIVIDE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_floor_divide (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_MODULO ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_MODULO"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_modulo (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_ADD ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_ADD"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_add (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_SUBTRACT ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_SUBTRACT"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_subtract (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BINARY_SUBSCR ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_SUBSCR"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_subtract (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))

    | INPLACE_MULTIPLY ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_MULTIPLY"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_multiply (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
      
    | INPLACE_FLOOR_DIVIDE ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_FLOOR_DIVIDE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_floor_divide (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
          
    | INPLACE_MODULO ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_MODULO"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_modulo (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
          
    | INPLACE_ADD ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_ADD"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_add (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | INPLACE_SUBTRACT ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "BINARY_SUBSCR"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = binary_subtract (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | LOAD_CONST(i) ->
      (match length frame.fCode.co_consts > i with
      | false -> 
        let newDataStack = [undefinedBehavior "LOAD_CONST"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = load_const i (frame.fCode.co_consts)
                           (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | LOAD_NAME(i) ->
      (match length frame.fCode.co_names > i with
      | false -> 
        let newDataStack = [undefinedBehavior "LOAD_NAME"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true -> 
        let newDataStack = load_name i (frame.fCode.co_names) (frame.f_locals)
                           (frame.f_globals) (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BUILD_TUPLE(i) ->
      (match length frame.dataStack >= i with
      | false ->
        let newDataStack = [undefinedBehavior "BUILD_TUPLE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true -> 
        let newDataStack = build_tuple i (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
     
    | STORE_NAME(i) ->
      (match length frame.fCode.co_names > i && length frame.dataStack >= 1 with
      | false ->
        let newDataStack = [undefinedBehavior "STORE_NAME"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newLocals, newDataStack = store_name i (frame.fCode.co_names)
                                      (frame.f_locals) (frame.dataStack) in
            execBytecode ({frame with dataStack = newDataStack; 
                                      pc = frame.pc+1;
                                      f_locals = newLocals}))

    |  BUILD_LIST(i) ->
      (match length frame.dataStack >= i with
      | false ->
        let newDataStack = [undefinedBehavior "BUILD_LIST"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = build_list i (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BUILD_MAP(i) ->
      (match length frame.dataStack >= (op_Multiply 2 i) with
      | false ->
        let newDataStack = [undefinedBehavior "BUILD_MAP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true -> 
        let newDataStack = build_map i (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | BUILD_CONST_KEY_MAP(i) ->
      (match length frame.dataStack >= i+1 with
      | false ->
        let newDataStack = [undefinedBehavior "BUILD_CONST_KEY_MAP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true -> 
        let newDataStack = build_const_key_map i (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
        
    | JUMP_FORWARD(i) -> execBytecode ({frame with pc = frame.pc + (i/2) + 1})
    
    | POP_JUMP_IF_TRUE(i) ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "POP_JUMP_IF_TRUE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newPc, newDataStack = pop_jump_if_true i (frame.pc)
                                                     (frame.dataStack) in
        let newPc = if frame.pc=newPc then newPc+1 else newPc in
          execBytecode ({frame with dataStack = newDataStack; pc = newPc}))
    
    | POP_JUMP_IF_FALSE(i) ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "POP_JUMP_IF_FALSE"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newPc, newDataStack = pop_jump_if_false i (frame.pc) 
                                                      (frame.dataStack) in
        let newPc = if frame.pc=newPc then newPc+1 else newPc in
          execBytecode ({frame with dataStack = newDataStack; pc = newPc}))
    
    | JUMP_IF_TRUE_OR_POP(i) ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "JUMP_IF_TRUE_OR_POP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newPc, newDataStack = jump_if_true_or_pop i (frame.pc)
                                                        (frame.dataStack) in
        let newPc = if frame.pc=newPc then newPc+1 else newPc in
          execBytecode ({frame with dataStack = newDataStack; pc = newPc}))
    
    | JUMP_IF_FALSE_OR_POP(i) ->
      (match frame.dataStack with
      | [] ->
        let newDataStack = [undefinedBehavior "JUMP_IF_FALSE_OR_POP"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newPc, newDataStack = jump_if_false_or_pop i (frame.pc) 
                                                         (frame.dataStack) in
        let newPc = if frame.pc=newPc then newPc+1 else newPc in
          execBytecode ({frame with dataStack = newDataStack; pc = newPc}))
    
    | FOR_ITER(i) ->
      (match frame.dataStack with
      | [] -> 
        let newDataStack = [undefinedBehavior "FOR_ITER"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | _ ->
        let newPc, newDataStack = for_iter i (frame.pc) (frame.dataStack) in
        let newPc = if frame.pc=newPc then newPc+1 else newPc in
          execBytecode ({frame with dataStack = newDataStack; pc = newPc}))
    
    | JUMP_ABSOLUTE(i) -> execBytecode ({frame with pc = i/2})
    
    | LOAD_GLOBAL(i) ->
      (match length frame.fCode.co_names > i with
      | false -> 
        let newDataStack = [undefinedBehavior "LOAD_GLOBAL"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = load_global i (frame.fCode.co_names) (frame.f_globals) 
                                         (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | LOAD_FAST(i) ->
      (match length frame.f_localplus > i with
      | false ->  
        let newDataStack = [undefinedBehavior "LOAD_FAST"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newDataStack = load_fast i (frame.f_localplus) (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}))
    
    | STORE_FAST(i) ->
      (match length frame.f_localplus >= i && length frame.dataStack >= 1 with
      | false -> 
        let newDataStack = [undefinedBehavior "STORE_FAST"] in
          execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
      | true ->
        let newLocalPlus, newDataStack = store_fast i (frame.f_localplus) 
                                                      (frame.dataStack) in
          execBytecode ({frame with dataStack = newDataStack;
                                    pc = frame.pc+1;
                                    f_localplus = newLocalPlus}))
                                
    | MAKE_FUNCTION(flags) ->
      (match length frame.dataStack >= 2 with
      | false ->
        let newDataStack = [undefinedBehavior "MAKE_FUNCTION_1"] in
          execBytecode {frame with dataStack = newDataStack; pc = frame.pc+1}
      | true ->
        (if flags <> 0 then
          (match length frame.dataStack >= 3 with 
          | false -> 
            let newDataStack = [undefinedBehavior "MAKE_FUNCTION_2"] in
              execBytecode {frame with dataStack = newDataStack; pc = frame.pc+1}
          | true -> 
             let newDataStack = make_function flags (frame.f_globals) (frame.dataStack) in
               execBytecode {frame with dataStack = newDataStack; pc = frame.pc+1})
         else
           let newDataStack = make_function flags (frame.f_globals) (frame.dataStack) in
             execBytecode {frame with dataStack = newDataStack; pc = frame.pc+1}))
    | _ -> 
      let newDataStack = [undefinedBehavior "INSTRUCTION_NOT_SUPPORTED"] in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})

(*
   - Runs the code in the frame and update the VM once it's done.
   - frame is top frame in call stack, and the caller frame is the top frame in virM
   Req: frame is the top element in the callstack in virM.
*)
let rec runFrame virM frame =
  (* Execute the top frame *) 
  let resultFrame = execBytecode frame in
  let result = List.hd resultFrame.dataStack in

  (* Check if we spawn a new frame*)
  match result with
  | FRAMEOBJECT newFrame ->
    let resultFrame = {resultFrame with dataStack = List.tail resultFrame.dataStack} in
    let newVM = {virM with callStack = resultFrame::(virM.callStack)} in
    runFrame newVM newFrame
  | _ -> 
    (* new global names*)
    let new_globals = resultFrame.f_globals in
    
    (* Check if the top frame is the global frame *)
    if List.length virM.callStack = 0
    then (virM, result) 
    else
      (* Push result to the top of the caller frame (top frame in the call stack) *)
      let callerFrame = List.hd virM.callStack in
      let newCallStack = List.tail virM.callStack in
      let newCallerFrame = {
        callerFrame with 
          dataStack =  result::callerFrame.dataStack;
          pc = callerFrame.pc + 1;
          f_globals = new_globals
      } in
      let newVM: vm = {virM with callStack = newCallStack} in
      runFrame newVM newCallerFrame
