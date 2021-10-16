module Exec

(*
   Req: length(datastack) >= 1
*)
let return_value dataStack = List.hd dataStack

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
  Req: length(datastack) >= 2
*)
let binary_add (dataStack: list pyObj) = 
  let a = List.hd dataStack in
  let b = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (a, b) with
  | INT(a'), INT(b') -> INT(a' + b')::newDataStack
  | STRING(a'), STRING(b') -> STRING(a' ^  b')::newDataStack
  | _, _ -> All.failwith "Error: binary_Add incompatable types"

(*
   Req: len(frame.fcode.bytecode) >= 1
*)
let rec execBytecode frame =
  let CODE(bc) = frame.fCode.co_code in
  match bc with
  | RETURN_VALUE::l -> return_value frame.dataStack
  | LOAD_CONST(i)::l ->
    let newDataStack = load_const i (frame.fCode.co_consts) (frame.dataStack) in
    execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}})
    
  | LOAD_FAST(i)::l ->
    let newDataStack = load_fast i (frame.fCode.co_varnames) (frame.dataStack) in
    execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}})   
    
  | STORE_FAST(i)::l ->
    let (newVarnames, newDataStack) = store_fast i (frame.fCode.co_varnames) (frame.dataStack) in
    let newFCode = {frame.fCode with co_code = CODE l; co_varnames = newVarnames} in
    execBytecode ({frame with dataStack = newDataStack; fCode = newFCode})
    
  | BINARY_ADD::l -> 
    let newDataStack = binary_add (frame.dataStack) in
    execBytecode ({frame with dataStack = newDataStack; fCode = {frame.fCode with co_code = CODE l}})
  | [] -> All.failwith "Error: reached empty bytecode array "

