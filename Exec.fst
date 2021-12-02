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
  let empty_map = Map.const_on (Set.empty) NONE in
  match List.hd code with
  | CODEOBJECT co ->  
    let newFrame: frameObj = {
      dataStack = []; 
      blockStack = [];
      fCode = co;
      f_localplus = localplus;
      pc = 0;
      f_globals = globals;
      f_locals = empty_map
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
     - if tos = INT(i) then res = INT(+i)
     - if tos = BOOL(b) then res = INT(if b then 1 else 0)
     - else ERR
*)
let unary_positive datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | INT i -> INT(i)::newDataStack
  | BOOL b -> INT(if b then 1 else 0)::newDataStack
  | _ -> ERR("Error: bad operand type for unary +")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens: 
     - if tos = INT(i) then res = INT(-i)
     - if tos = BOOL(b) then res = INT(if b then -1 else 0)
     - else ERR
*)
let unary_negative datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | INT i -> INT(-i)::newDataStack
  | BOOL b -> INT(if b then -1 else 0)::newDataStack
  | _ -> ERR("Error: bad operand type for unary -")::newDataStack

(*
   Req: length(datastack) >= 1
   Ens:
     - if tos = INT(i)
     - if tos = STRING(s)
     - if tos = BOOL(b)
     - if tos = NONE 
*)
let unary_not datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | INT i -> BOOL(if i=0 then true else false)::newDataStack
  | STRING s -> BOOL(if s="" then true else false)::newDataStack
  | BOOL b -> BOOL(if b then false else true)::newDataStack
  | LIST l -> BOOL(match l with [] -> true | _ -> false)::newDataStack
  | TUPLE t -> BOOL(match t with [] -> true | _ -> false)::newDataStack
  | NONE -> BOOL(true)::newDataStack
  | _ -> ERR("Error: bad operand type for unary -")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens:
    - if tos and tos1 are int(i1), int(i2) or bool(b1), bool(b2):
      - res = i1*i2, or res = b1 * b2
    - if tos and tos1 are List(l), int(i) or List(l), bool(b):
      - res = a list of the items of l repeated i (or b) times
      - if i <= 0 then res = []
    - else: res = ERR
*)
let binary_multiply (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(i1), INT(i2) -> INT(op_Multiply i2 i1)::newDataStack
  | BOOL(b1), BOOL(b2) -> INT(if b1&&b2 then 1 else 0)::newDataStack
  | BOOL(b), INT(i) -> INT(if b then i else  0)::newDataStack
  | INT(i), BOOL(b) -> INT(if b then i else 0)::newDataStack
  | LIST(l), INT(i) ->
    if 0 >= i 
    then LIST([])::newDataStack
    else
      let l' = List.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) i) in
      LIST(l')::newDataStack
  | INT(i), LIST(l) -> 
    if 0 >= i 
    then LIST([])::newDataStack
    else
      let l' = List.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> l) i) in
      LIST(l')::newDataStack
  | LIST(l), BOOL(b) -> if b then LIST(l)::newDataStack else LIST([])::newDataStack
  | BOOL(b), LIST(l) -> if b then LIST(l)::newDataStack else LIST([])::newDataStack
  | TUPLE(t), INT(i) ->
    if 0 >= i 
    then TUPLE([])::newDataStack
    else
      let t' = List.fold_left (fun x y -> List.append x y) ([]) (tabulate (fun x -> t) i) in
      TUPLE(t')::newDataStack
  | TUPLE(t), BOOL(b) -> if b then TUPLE(t)::newDataStack else TUPLE([])::newDataStack  
  | _, _ -> ERR("Error: unsupported operand type(s) for *")::newDataStack

(*
  Req: length(datastack) >= 2
  ENS:
    - if tos and tos1 are int(a), int(b) or bool(b1), bool(b2)
      - res = b // a (or b2 // b1) if a <> 0 (or b1 <> 0)
      - res = ERR if a == 0 (or b1 == 0)
*)
let binary_floor_divide (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(a), INT(b) -> (if a=0 then ERR("Error: division by zero") else INT(b/a))::newDataStack
  | BOOL(b1), BOOL(b2) -> (if not b1 then ERR("Error: division by zero") else if b2 then INT(1/1) else INT(0/1))::newDataStack
  | INT(i), BOOL(b2) -> (if i=0 then ERR("Error: division by zero") else if b2 then INT(1/i) else INT(0/i))::newDataStack
  | BOOL(b1), INT(i) -> (if not b1 then ERR("Error: division by zero") else INT(i/1))::newDataStack
  | _, _ -> ERR("Error: unsupported operand type(s) for //")::newDataStack

(*
  Req: length(datastack) >= 2
  ENS:
    - if tos and tos1 are int(a), int(b) or bool(b1), bool(b2)
      - res = b % a (or b2 % b1) if a <> 0 (or b1 <> 0)
      - res = ERR if a == 0 (or b1 == 0)
*)
let binary_modulo (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(a), INT(b) -> (if a=0 then ERR("Error: modulo by zero") else INT(b%a))::newDataStack
  | BOOL(b1), BOOL(b2) -> (if not b1 then ERR("Error: modulo by zero") else if b2 then INT(1%1) else INT(0%1))::newDataStack
  | INT(i), BOOL(b2) -> (if i=0 then ERR("Error: modulo by zero") else if b2 then INT(1%i) else INT(0%i))::newDataStack
  | BOOL(b1), INT(i) -> (if not b1 then ERR("Error: modulo by zero") else INT(i%1))::newDataStack
  | _, _ -> ERR("Error: unsupported operand type(s) for %")::newDataStack

(*
  Req: length(datastack) >= 2
  Ens: 
    - if tos and tos1 are INT(i1)/BOOL(b1) and INT(i2)/BOOL(b2):
      - res = INT(i1 + i2) (or b1,b2)
    - if tos and tos1 are STRING(s1) and STRING(s2):
      - res = STRING(s1 ^ s2)
    - if tos and tos1 are LIST(l1) and LIST(l2):
      - res = LIST(l1 @ l2)
    - if tos and tos1 are TUPLE(t1) and TUPLE(t2):
      - res = TUPLE(t1 @ t2)
    - else:
      - res = ERR
*)
let binary_add (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(a), INT(b) -> INT(b + a)::newDataStack
  | BOOL(b1), BOOL(b2) -> INT(if b1&&b2 then 2 else if b1||b2 then 1 else 0)::newDataStack
  | BOOL(b1), INT(i) -> INT(if b1 then 1+i else  0+i)::newDataStack
  | INT(i), BOOL(b2) -> INT(if b2 then 1+i else 0+i)::newDataStack
  | STRING(s1), STRING(s2) -> STRING(s2 ^  s1)::newDataStack
  | LIST(l1), LIST(l2) -> LIST(List.append l1 l2)::newDataStack
  | TUPLE(t1), TUPLE(t2) -> LIST(List.append t1 t2)::newDataStack
  | _, _ -> ERR("Error: unsupported operand type(s) for +")::newDataStack
 
(*
  Req: length(datastack) >= 2
  Ens:
    - if tos and tos1 are INT(i1) and INT(i2) or(BOOL(b1) and BOOL(b2)):
      - res = INT(i2 - i1), or(b1, b2)
    - else:
      - res = ERR
*)
let binary_subtract (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(a), INT(b) -> INT(b - a)::newDataStack
  | BOOL(b1), BOOL(b2) -> INT(if b1&&b2 then 0 else if b1 then -1 else if b2 then 1 else 0)::newDataStack
  | BOOL(b1), INT(i) -> INT(if b1 then i-1 else  i-0)::newDataStack
  | INT(i), BOOL(b2) -> INT(if b2 then 1+i else 0+i)::newDataStack
  | _, _ -> ERR("Error: unsupported operand type(s) for -")::newDataStack
  
(*
  Req: length(datastack) >= 2
  Ens: TOS = TOS1[TOS]
*)
let binary_subscr (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos1, tos) with
  | STRING(s), INT(i) ->
    let c = if i >= 0 then (subString_pos s i) else (subString_neg s i) in
     ( match c with
      | Some c' -> STRING(c')::newDataStack
      | None -> ERR("Error: String index out of range")::newDataStack)
  | STRING(s), BOOL(b) -> 
    let c = if b then (subString_pos s 1) else (subString_neg s 0) in
      (match c with
      | Some c' -> STRING(c')::newDataStack
      | None -> ERR("Error: String index out of range")::newDataStack)
  | LIST(l), INT(i) -> 
    let e = 
      if i >= 0 then (List.Tot.Base.nth l i)
      else if (-i > List.length l) then None
      else (List.Tot.Base.nth l ((List.length l) - i))
    in
     ( match e with
      | Some e' -> e'::newDataStack
      | None ->  ERR("Error: List index out of range")::newDataStack)
  | LIST(l), BOOL(b) ->
    let e = if b then (List.Tot.Base.nth l 1) else (List.Tot.Base.nth l 0) in
     ( match e with
      | Some e' -> e'::newDataStack
      | None -> ERR("Error: List index out of range")::newDataStack)
  | TUPLE(t), INT(i) ->
    let e = 
      if i >= 0 then (List.Tot.Base.nth t i)
      else if (-i > List.length t) then None
      else (List.Tot.Base.nth t ((List.length t) - i))
    in
     ( match e with
      | Some e' -> e'::newDataStack
      | None ->  ERR("Error: Tuple index out of range")::newDataStack)
  | TUPLE(t), BOOL(b) ->
    let e = if b then (List.Tot.Base.nth t 1) else (List.Tot.Base.nth t 0) in
      (match e with
      | Some e' -> e'::newDataStack
      | None -> ERR("Error: Tuple index out of range")::newDataStack)
  | _, _ -> ERR("Error: object is not subscriptable")::newDataStack
  
(*
  Req:
  Ens:
*)
let store_name i names f_locals dataStack = 
  let name = List.nth names i in
  let tos = List.hd dataStack in
  let _, newDataStack = List.splitAt 1 dataStack in 
  (match name with
  | STRING s -> 
    let newLocals = Map.upd f_locals s tos in
    (newLocals, newDataStack)
  | _ -> All.failwith "STORE_NAME: co_names[i] is not STRING")
  
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
  match name with
  | STRING s ->
    (match Map.contains f_locals s with
     | true -> (Map.sel f_locals s)::dataStack 
     | false ->
       (match Map.contains f_globals s with
        | true -> (Map.sel f_locals s)::dataStack
        | false -> (ERR ("name " ^ s ^ "is not defined"))::dataStack
       )
    )
  | _ -> All.failwith "LOAD_NAME: co_names[i] is not STRING"
 
(*
  Req: (length(datastack) >= i)
  Ens: res = LIST([dataStack[0]; dataStack[1], ... dataStrack[i-1]])::newDataStack
*)
let build_tuple i dataStack =
  let elems, newDataStack  = List.splitAt i dataStack in
  let newDataStack = TUPLE(elems)::newDataStack in
  newDataStack

(*
  Req: (length(datastack) >= i)
  Ens: res = TUPLE([dataStack[0]; dataStack[1], ... dataStrack[i-1]])::newDataStack
*)
let build_list i dataStack =
  let elems, newDataStack  = List.splitAt i dataStack in
  let newDataStack = LIST(elems)::newDataStack in
  newDataStack

(*
  Req:
  Ens:
*)
let pop_jump_if_true i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | BOOL b -> if b then (i/2, newDataStack) else (pc, newDataStack)
  | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack)

(*
  Req:
  Ens:
*)
let pop_jump_if_false i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | BOOL b -> if b then (pc, newDataStack) else (i/2, newDataStack)
  | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_true_or_pop i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | BOOL b -> if b then (i/2, dataStack) else (pc, newDataStack)
  | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack)

(*
  Req:
  Ens:
*)
let jump_if_false_or_pop i pc dataStack =
  let tos = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  match tos with
  | BOOL b -> if b then (pc, newDataStack) else (i/2, dataStack)
  | _ -> (pc, (ERR "ERR: argument is not a Bool")::newDataStack)
 
(*
  Req:
  Ens:
*)
let load_global i names f_globals dataStack =
  let name = List.nth names i in
  match name with
  | STRING s ->
    (match Map.contains f_globals s with
     | true -> (Map.sel f_globals s)::dataStack 
     | false -> (ERR ("name " ^ s ^ "is not defined"))::dataStack
    )
  | _ -> All.failwith "LOAD_GLOBAL: co_names[i] is not STRING"

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
  let func = FUNCTION ({
    func_Code = codeobj;
    func_globals = globs;
    func_name = qualname;
    func_closure =
      if flags = 8 then
      (match List.hd newDataStack with
       | TUPLE(t) -> TUPLE(t)
       | _ -> ERR "Expected closure tuple, got someting else") else NONE;
   
    func_defaults =
      if flags = 1 then
      (match  List.hd newDataStack with
       | TUPLE(t) -> TUPLE(t)
       | _ -> ERR "Expected a defaults tuple, got someting else") else NONE;
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
   | _ -> All.failwith "daa"

    
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
