module Exec

let builtinsToPyObj (bltin: builtins) = 
  match bltin with
  | INT n -> createInt n
  | STRING s -> createString s
  | BOOL b -> createBool b
  | LIST l -> createList l
  | TUPLE t -> createTuple t
  | DICT kvl -> createDict kvl 
  | FUNCTION f -> createFunction f
  | NONE -> createNone

 
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

(* Create a new frame and push it in the frame stack *)
let makeFrame virM code localplus global_names local_names =
  let frame: frameObj = {
    dataStack = []; 
    blockStack = [];
    fCode = code;
    f_localplus = localplus;
    pc = 0;
    f_globals = global_names;
    f_locals = local_names
  } in
  let newVM: vm = {virM with callStack = frame::(virM.callStack)} in
  (newVM, frame)

(*
  Req:
  Ens:
*)
let call_function i globals dataStack =
  let args, newDataStack = List.splitAt i dataStack in
  let localplus = List.Tot.Base.rev args in
  let code, newDataStack = List.splitAt 1 newDataStack in
  match List.hd code with
  | CODEOBJECT co ->  
    let newFrame: frameObj = {
      dataStack = []; 
      blockStack = [];
      fCode = co;
      f_localplus = localplus;
      pc = 0;
      f_globals = globals;
      f_locals = emptyMap
    } in (FRAMEOBJECT newFrame)::dataStack
  | _ -> All.failwith "CALL_FUNCTION: didn't call a function object"

(*
   Req: length(datastack) >= 1
   Ens: res = datastack[1:]
*)
let pop_top datastack = List.tail datastack

(*
   Req: length(datastack) >= 2
   Ens: res = tos1::tos::datastack[2:]
*)
let rot_two datastack = 
  let tos = List.hd datastack in
  let tos1 = List.nth datastack 1 in
  let _, rest_stack = List.splitAt 2 datastack in
  tos1::tos::rest_stack

(*
   Req: length(datastack) >= 3
   Ens: res = tos1::tos2::tos::datastack[3:]
*)
let rot_three datastack = 
  let tos = List.hd datastack in
  let tos1 = List.nth datastack 1 in
  let tos2 = List.nth datastack 2 in
  let _, rest_stack = List.splitAt 3 datastack in
  tos1::tos2::tos::rest_stack

(*
   Req: length(datastack) >= 4
   Ens: res = tos1::tos2::tos3::tos::datastack[4:]
*)
let rot_four datastack = 
  let tos = List.hd datastack in
  let tos1 = List.nth datastack 1 in
  let tos2 = List.nth datastack 2 in
  let tos3 = List.nth datastack 3 in
  let _, rest_stack = List.splitAt 4 datastack in
  tos1::tos2::tos3::tos::rest_stack

(*
   Req: length(datastack) >= 1
   Ens: res = tos::datastack
*)
let dup_top datastack = 
  let tos = List.hd datastack in
  tos::datastack

(*
   Req: length(datastack) >= 2
   Ens: res = tos::tos1::datastack
*)
let dup_top_two datastack = 
  let tos = List.hd datastack in
  let tos1 = List.nth datastack 1 in
  tos::tos1::datastack

(*
   Req: length(datastack) >= 1
   Ens:
*)
let unary_positive datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | PYTYP(OBJ(obj)) -> 
    (match (Map.sel (obj.methods) "__pos__") with
    | UNFUN f -> 
      (match f (OBJ obj) with
      | NONE -> ERR("Can't positive  " ^ obj.name)::newDataStack 
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _ -> ERR("Cannot positive non-objects")::newDataStack
  
(*
   Req: length(datastack) >= 1
   Ens:
*)
let unary_negative datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | PYTYP(OBJ(obj)) -> 
    (match (Map.sel (obj.methods) "__neg__") with
    | UNFUN f -> 
      (match f (OBJ obj) with
      | NONE -> ERR("Can't negate  " ^ obj.name)::newDataStack 
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _ -> ERR("Cannot negate non-objects")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens:
*)
let unary_not datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | PYTYP(OBJ(obj)) ->
    let res = 
      (match obj.value with
      | INT i -> if i=0 then true else false
      | STRING s -> if s="" then true else false
      | BOOL b -> if b then false else true
      | LIST l -> (match l with [] -> true | _ -> false)
      | TUPLE t -> (match t with [] -> true | _ -> false)
      | DICT kvl -> (match kvl with [] -> true | _ -> false)
      | FUNCTION f -> false
      | NONE -> true) in (createBool res)::newDataStack
  | _ -> ERR("Cannot logically negate non-objects")::newDataStack
  
(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_multiply (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__mul__") with
    | BINFUN f -> 
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot multiply " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack 
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot multiply non-objects")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_floor_divide (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__floordiv__") with
    | BINFUN f -> 
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot floor-divide " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack 
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot floor-divide non-objects")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_modulo (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__mod__") with
    | BINFUN f -> 
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot mod " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot mod non-objects")::newDataStack


(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_add (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__add__") with
    | BINFUN f -> 
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot add " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot add non-objects")::newDataStack


(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_subtract (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__sub__") with
    | BINFUN f -> 
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot subtract " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot subtract non-objects")::newDataStack


(*
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_subscr (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos1, tos) with
  | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
    (match (Map.sel (obj1.methods) "__subscr__") with
    | BINFUN f ->
      (match f (OBJ obj1, OBJ obj2) with
      | NONE -> ERR("Cannot subscript " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack 
      | bltin -> (builtinsToPyObj bltin)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot subscript non-objects")::newDataStack


(*
  Req: (length(datastack) >= i)
  Ens:
*)
let build_tuple i dataStack =
  let elems, newDataStack  = List.splitAt i dataStack in
  let newDataStack = createTuple(List.map pyObjTopyTyp elems)::newDataStack in
  newDataStack

(*
  Req: (length(datastack) >= i)
  Ens:
*)
let build_list i dataStack =
  let elems, newDataStack  = List.splitAt i dataStack in
  let newDataStack = createList(List.map pyObjTopyTyp elems)::newDataStack in
  newDataStack

(*
  Req:
  Ens:
*)
let compare_op i dataStack = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in
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
  | "__is__" ->
    (match (tos1, tos) with
    | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> createBool(obj1.pid = obj2.pid)::newDataStack
    | _ -> (ERR "Cannot compare non-object")::newDataStack) 
  | "__nis__" ->
    (match (tos1, tos) with
    | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> createBool(obj1.pid <> obj2.pid)::newDataStack
    | _ -> (ERR "Cannot compare non-object")::newDataStack)
  | "error" -> (ERR "Compare_op is not supported")::newDataStack
  | _ ->
    match (tos1, tos) with
    | PYTYP(OBJ(obj1)), PYTYP(OBJ(obj2)) -> 
      (match (Map.sel (obj1.methods) op) with
      | BINFUN f -> 
        (match f (OBJ obj1, OBJ obj2) with
        | NONE ->
          (match i with 
          | 2 -> (createBool false)::newDataStack
          | 3 -> (createBool false)::newDataStack
          | _ -> ERR("Cannot compare " ^ obj1.name ^ " and " ^ obj2.name)::newDataStack)
        | bltin -> (builtinsToPyObj bltin)::newDataStack)
      | err -> err::newDataStack)
    | _, _ -> ERR("Cannot compare non-objects")::newDataStack

(*
  Req:
  Ens:
*)
let store_name i names f_locals dataStack = 
  let name = List.nth names i in
  let tos = List.hd dataStack in
  let _, newDataStack = List.splitAt 1 dataStack in 
  let newLocals = Map.upd f_locals name tos in
  (newLocals, newDataStack)
  
  
(*
  Req: (length(consts) > i) && (length(datastack) >= 1)
  Ens: res = consts[i]::datastack
*)
let load_const i consts dataStack =
  let elem = List.nth consts i in
  let newDataStack = elem::dataStack in
  newDataStack

(*
  Req:
  Ens:
*)
let load_name i names f_locals f_globals dataStack =
  let name = List.nth names i in
  match Map.contains f_locals name with
   | true -> (Map.sel f_locals name)::dataStack 
   | false ->
     (match Map.contains f_globals name with
      | true -> (Map.sel f_locals name)::dataStack
      | false -> (ERR ("name: " ^ name ^ "is not defined"))::dataStack)

(*
  Req:
  Ens:
*)
let pop_jump_if_true i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | PYTYP(OBJ(obj)) ->
    (match obj.value with
    | BOOL b -> if b then (i/2, newDataStack) else (pc, newDataStack)
    | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack))
  | _ -> (pc, (ERR "ERR: argument is not an object")::newDataStack)

(*
  Req:
  Ens:
*)
let pop_jump_if_false i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | PYTYP(OBJ(obj)) ->
    (match obj.value with
    | BOOL b -> if b then (pc, newDataStack) else (i/2, newDataStack)
    | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack))
  | _ -> (pc, (ERR "ERR: argument is not an object")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_true_or_pop i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | PYTYP(OBJ(obj)) ->
    (match obj.value with
    | BOOL b -> if b then (i/2, dataStack) else (pc, newDataStack)
    | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack))
  | _ -> (pc, (ERR "ERR: argument is not an object")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_false_or_pop i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | PYTYP(OBJ(obj)) ->
    (match obj.value with
    | BOOL b -> if b then (pc, newDataStack) else (i/2, dataStack)
    | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack))
  | _ -> (pc, (ERR "ERR: argument is not an object")::newDataStack)

(*
  Req:
  Ens:
*)
let load_global i names f_globals dataStack =
  let name = List.nth names i in
  match Map.contains f_globals name with
   | true -> (Map.sel f_globals name)::dataStack 
   | false -> (ERR ("name: " ^ name ^ "is not defined"))::dataStack

(*
  Req: (length(localplus) > i) && (length(datastack) >= 1)
  Ens: res = localplus[i]::dataStack
*)
let load_fast i localPlus dataStack =
  let elem = List.nth localPlus i in
  let newDataStack = elem::dataStack in
  newDataStack

(*
  Req: i >= (length(f_localplus)) && (length(datastack) >= 1)
  Ens: res = (localplus where localplus[i] = tos, datastack[1:])
*)
let store_fast i localplus dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
    match (List.Tot.Base.length localplus) > i with
    | true ->
      (let newLocalPlus = List.mapi (fun k x -> if k = i then tos else x) localplus in
        (newLocalPlus, newDataStack))
    | false ->
      (match (List.Tot.Base.length localplus) = i with
       | true ->
         (let newLocalPlus = List.append localplus [tos] in
           (newLocalPlus, newDataStack)) 
       | false -> All.failwith "STORE_FAST: storing out of index")

(*
  Req:
  Ens:
*)
let make_function flags globs dataStack =
  let qualname = List.hd dataStack in
  let codeobj = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in
  let func = createFunction ({
    func_Code = codeobj;
    func_globals = globs;
    func_name = qualname;
    func_closure =
      if flags = 8 then
      (match List.hd newDataStack with
       | PYTYP(OBJ(obj)) ->
         (match obj.value with
           | TUPLE(t) -> createTuple t
           | _ -> ERR "Expected closure tuple, got someting else")
       | _ -> ERR "Expected closure tuple, got someting else") else createNone;
   
    func_defaults =
      if flags = 1 then
      (match  List.hd newDataStack with
      | PYTYP(OBJ(obj)) ->
         (match obj.value with
           | TUPLE(t) -> createTuple t
           | _ -> ERR "Expected a defaults tuple, got someting else")
       | _ -> ERR "Expected a defaults tuple, got someting else") else createNone;
  }) in
    (match flags with
     | 0 -> func::newDataStack
     | _ -> func::(List.tail newDataStack))

(*
   Req: len(frame.fcode.bytecode) >= 1
*)
let rec execBytecode frame =
  match check_err frame.dataStack with
  | Some err -> frame
  | None -> 
  
    let CODE(bc) = frame.fCode.co_code in
    match List.nth bc (frame.pc) with
    | RETURN_VALUE -> frame
    | CALL_FUNCTION i -> 
      let newDataStack = call_function i (frame.f_globals) frame.dataStack in
        ({frame with dataStack = newDataStack})
    | NOP -> execBytecode ({frame with pc = frame.pc+1})
    | POP_TOP ->
      let newDataStack = pop_top frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | ROT_TWO ->
      let newDataStack = rot_two frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | ROT_THREE ->
      let newDataStack = rot_three frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | ROT_FOUR ->
      let newDataStack = rot_four frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | DUP_TOP ->
      let newDataStack = dup_top frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | DUP_TOP_TWO ->
      let newDataStack = dup_top_two frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | UNARY_POSITIVE ->
      let newDataStack = unary_positive frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | UNARY_NEGATIVE ->
      let newDataStack = unary_negative frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | UNARY_NOT ->
      let newDataStack = unary_not frame.dataStack in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | BINARY_MULTIPLY -> 
      let newDataStack = binary_multiply (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | BINARY_FLOOR_DIVIDE -> 
      let newDataStack = binary_floor_divide (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})    
    | BINARY_MODULO -> 
      let newDataStack = binary_modulo (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | BINARY_ADD -> 
      let newDataStack = binary_add (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})    
    | BINARY_SUBTRACT -> 
      let newDataStack = binary_subtract (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}) 
    | BINARY_SUBSCR -> 
      let newDataStack = binary_subtract (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1}) 
    | LOAD_CONST(i) ->
      let newDataStack = load_const i (frame.fCode.co_consts) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | LOAD_NAME(i) ->
      let newDataStack = load_name i (frame.fCode.co_names) (frame.f_locals) (frame.f_globals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | BUILD_TUPLE(i) ->
      let newDataStack = build_tuple i (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | STORE_NAME(i) ->
      let newLocals, newDataStack = store_name i (frame.fCode.co_names) (frame.f_locals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1; f_locals = newLocals})
    |  BUILD_LIST(i) ->
      let newDataStack = build_list i (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | JUMP_FORWARD(i) -> execBytecode ({frame with pc = frame.pc + (i/2) + 1})
    | POP_JUMP_IF_TRUE(i) ->
      let newPc, newDataStack = pop_jump_if_true i (frame.pc) (frame.dataStack) in
      let newPc = if frame.pc=newPc then newPc+1 else newPc in
        execBytecode ({frame with dataStack = newDataStack; pc = newPc})
    | POP_JUMP_IF_FALSE(i) ->
      let newPc, newDataStack = pop_jump_if_false i (frame.pc) (frame.dataStack) in
      let newPc = if frame.pc=newPc then newPc+1 else newPc in
        execBytecode ({frame with dataStack = newDataStack; pc = newPc})
    | JUMP_IF_TRUE_OR_POP(i) ->
      let newPc, newDataStack = jump_if_true_or_pop i (frame.pc) (frame.dataStack) in
      let newPc = if frame.pc=newPc then newPc+1 else newPc in
        execBytecode ({frame with dataStack = newDataStack; pc = newPc})
    | JUMP_IF_FALSE_OR_POP(i) ->
      let newPc, newDataStack = jump_if_false_or_pop i (frame.pc) (frame.dataStack) in
      let newPc = if frame.pc=newPc then newPc+1 else newPc in
        execBytecode ({frame with dataStack = newDataStack; pc = newPc})
    | JUMP_ABSOLUTE(i) -> execBytecode ({frame with pc = i/2})
    | LOAD_GLOBAL(i) ->
      let newDataStack = load_global i (frame.fCode.co_names) (frame.f_globals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | LOAD_FAST(i) ->
      let newDataStack = load_fast i (frame.f_localplus) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})   
    | STORE_FAST(i) ->
      let newLocalPlus, newDataStack = store_fast i (frame.f_localplus) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1; f_localplus = newLocalPlus})
    | MAKE_FUNCTION(flags) ->
       let newDataStack = make_function flags (frame.f_globals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
   | _ -> All.failwith "Bytecode instruction is not implemented yet"

(*
   - Runs the code in the frame and update the VM once it's done.
   Req: frame is the top element in the callstack in virM.
*)
let rec runFrame virM frame =
  (* Execute the top frame *) 
  let resultFrame = execBytecode frame in
  let result = List.hd resultFrame.dataStack in

  (* Check if we spawn a new frame*)
  match result with
  | FRAMEOBJECT newFrame ->
    let resultStack = {resultFrame with dataStack = List.tail resultFrame.dataStack} in
    let newVM = {virM with callStack = resultFrame::(virM.callStack)} in
    runFrame newVM newFrame
  
  | _ -> 
    (* new global names*)
    let new_globals = resultFrame.f_globals in
    (* Pop the top frame *)
    let popVM = {virM with callStack = List.tail (virM.callStack)} in
    
    (* Check if the top frame is the global frame *)
    if List.length popVM.callStack = 0
    then (popVM, result) 
    else
      (* Push result to the top of the caller frame (top frame in the call stack) *)
      let callerFrame = List.hd popVM.callStack in
      let newCallStack = List.tail popVM.callStack in
      let newCallerFrame = {
        callerFrame with 
          dataStack =  result::callerFrame.dataStack;
          pc = callerFrame.pc + 1;
          f_globals = new_globals
      } in
      let newVM: vm = {popVM with callStack = newCallerFrame::newCallStack} in
      runFrame newVM newCallerFrame
