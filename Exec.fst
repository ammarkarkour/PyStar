module Exec

(*
   Req: true
*)
let check_err dataStack = 
  match (List.length dataStack) with
  | 0 -> None
  | _ ->
    match (List.hd dataStack) with
    | ERR(s) -> Some (ERR(s))
    | _ -> None
    
(*
   Req: length(datastack) >= 1
*)
let return_value dataStack = List.hd dataStack

(*
   Req: length(datastack) >= 1
*)
let pop_top datastack = List.tail datastack

(*
   Req: length(datastack) >= 2
*)
let rot_two datastack = 
  let fst_elem = List.hd datastack in
  let scnd_elem = List.nth datastack 1 in
  let _, rest_stack = List.splitAt 2 datastack in
  scnd_elem::fst_elem::rest_stack

(*
   Req: length(datastack) >= 3
*)
let rot_three datastack = 
  let fst_elem = List.hd datastack in
  let scnd_elem = List.nth datastack 1 in
  let thrd_elem = List.nth datastack 2 in
  let _, rest_stack = List.splitAt 3 datastack in
  scnd_elem::thrd_elem::fst_elem::rest_stack

(*
   Req: length(datastack) >= 4
*)
let rot_four datastack = 
  let fst_elem = List.hd datastack in
  let scnd_elem = List.nth datastack 1 in
  let thrd_elem = List.nth datastack 2 in
  let frth_elem = List.nth datastack 3 in
  let _, rest_stack = List.splitAt 4 datastack in
  scnd_elem::thrd_elem::frth_elem::fst_elem::rest_stack

(*
   Req: length(datastack) >= 1
*)
let dup_top datastack = 
  let fst_elem = List.hd datastack in
  fst_elem::datastack

(*
   Req: length(datastack) >= 2
*)
let dup_top_two datastack = 
  let fst_elem = List.hd datastack in
  let scnd_elem = List.nth datastack 1 in
  fst_elem::scnd_elem::datastack


(*
   Req: length(datastack) >= 1
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
*)
let unary_not datastack = 
  let tos = List.hd datastack in
  let newDataStack = List.tail datastack in
  match tos with
  | INT i -> BOOL(if i=0 then true else false)::newDataStack
  | STRING s -> BOOL(if s="" then true else false)::newDataStack
  | BOOL b -> BOOL(if b then false else true)::newDataStack
  | NONE -> BOOL(true)::newDataStack
  | _ -> ERR("Error: bad operand type for unary -")::newDataStack

(*
  Req: length(datastack) >= 2
*)
let binary_multiply (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | INT(i1), INT(i2) -> INT(op_Multiply i2 i1)::newDataStack
  | BOOL(b1), BOOL(b2) -> INT(if b1&&b2 then 1 else 0)::newDataStack
  | BOOL(b1), INT(i) -> INT(if b1 then i else  0)::newDataStack
  | INT(i), BOOL(b2) -> INT(if b2 then i else 0)::newDataStack
  | _, _ -> ERR("Error: unsupported operand type(s) for *")::newDataStack

(*
  Req: length(datastack) >= 2
  ENS: (tos = tos1 // tos) or (tos = ERR)
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
  ENS: (tos = tos1 % tos) or (tos = ERR)
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
  | _, _ -> ERR("Error: unsupported operand type(s) for +")::newDataStack
  
(*
  Req: length(datastack) >= 2
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
  Req: (length(consts) > i) && (length(datastack) >= 1)
*)
let load_const i consts dataStack =
  let elem = List.nth consts i in
  let newDataStack = elem::dataStack in
  newDataStack

(*
  Req: (length(varnames) > i) && (length(datastack) >= 1)
*)
let load_fast i varnames dataStack =
  let elem = List.nth varnames i in
  let newDataStack = elem::dataStack in
  newDataStack

(*
  Req: (length(varnames) > i) && (length(datastack) >= 1)
*)
let store_fast i varnames dataStack =
  let elem = List.hd dataStack in
  let newDataStack = List.tail dataStack in
  let newVarnames = List.mapi (fun k x -> if k = i then elem else x) varnames in
  (newVarnames, newDataStack)

(*
   Req: len(frame.fcode.bytecode) >= 1
*)
let rec execBytecode frame =
  let CODE(bc) = frame.fCode.co_code in
  match bc with
  | RETURN_VALUE::l -> return_value frame.dataStack

  | NOP::l -> execBytecode ({frame with fCode = {frame.fCode with co_code = CODE l}})

  | POP_TOP::l ->
    let newDataStack = pop_top frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
  
  | ROT_TWO::l ->
    let newDataStack = rot_two frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
   
  | ROT_THREE::l ->
    let newDataStack = rot_three frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
   
  | ROT_FOUR::l ->
    let newDataStack = rot_four frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | DUP_TOP::l ->
    let newDataStack = dup_top frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | DUP_TOP_TWO::l ->
    let newDataStack = dup_top_two frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | UNARY_POSITIVE::l ->
    let newDataStack = unary_positive frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | UNARY_NEGATIVE::l ->
    let newDataStack = unary_negative frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | UNARY_NOT::l ->
    let newDataStack = unary_not frame.dataStack in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))

  | BINARY_MULTIPLY::l -> 
    let newDataStack = binary_multiply (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
    
  | BINARY_FLOOR_DIVIDE::l -> 
    let newDataStack = binary_floor_divide (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))    

  | BINARY_MODULO::l -> 
    let newDataStack = binary_modulo (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
    
  | BINARY_ADD::l -> 
    let newDataStack = binary_add (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))    

  | BINARY_SUBTRACT::l -> 
    let newDataStack = binary_subtract (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}})) 
    
  | LOAD_CONST(i)::l ->
    let newDataStack = load_const i (frame.fCode.co_consts) (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))
    
  | LOAD_FAST(i)::l ->
    let newDataStack = load_fast i (frame.fCode.co_varnames) (frame.dataStack) in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None ->  execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}}))   
    
  | STORE_FAST(i)::l ->
    let (newVarnames, newDataStack) = store_fast i (frame.fCode.co_varnames) (frame.dataStack) in
    let newFCode = {frame.fCode with co_code = CODE l; co_varnames = newVarnames} in
    let errCheck = check_err newDataStack in (
    match errCheck with
    | Some err -> err
    | None -> execBytecode ({frame with dataStack = newDataStack; fCode = newFCode}))
    
    | [] -> All.failwith "Error: reached empty bytecode array "

