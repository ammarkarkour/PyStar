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
  Req: length(datastack) >= 2
  Ens: 
*)
let binary_add (dataStack: list pyObj) = 
  let tos = List.hd dataStack in
  let tos1 = List.nth dataStack 1 in
  let (_, newDataStack) = List.splitAt 2 dataStack in 
  match (tos, tos1) with
  | TYP(obj1), TYP(obj2) -> 
    (match (Map.sel (obj1.methods) "+") with
    | BINFUN f -> 
      (match f (obj1.value, obj2.value) with
      | bltin -> (builtinsToPyObj bltin)::newDataStack
      | NONE -> ERR("Cannot add objects" ^ obj1.name ^ obj2.name)::newDataStack)
    | err -> err::newDataStack)
  | _, _ -> ERR("Cannot add non objects")::newDataStack

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
   Req: len(frame.fcode.bytecode) >= 1
*)
let rec execBytecode frame =
  match check_err frame.dataStack with
  | Some err -> frame
  | None -> 
  
    let CODE(bc) = frame.fCode.co_code in
    match List.nth bc (frame.pc) with
    | RETURN_VALUE -> frame
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
    | BINARY_ADD -> 
      let newDataStack = binary_add (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | STORE_NAME(i) ->
      let newLocals, newDataStack = store_name i (frame.fCode.co_names) (frame.f_locals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1; f_locals = newLocals})
    | LOAD_CONST(i) ->
      let newDataStack = load_const i (frame.fCode.co_consts) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | LOAD_NAME(i) ->
      let newDataStack = load_name i (frame.fCode.co_names) (frame.f_locals) (frame.f_globals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | LOAD_GLOBAL(i) ->
      let newDataStack = load_global i (frame.fCode.co_names) (frame.f_globals) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})
    | LOAD_FAST(i) ->
      let newDataStack = load_fast i (frame.f_localplus) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1})   
    | STORE_FAST(i) ->
      let newLocalPlus, newDataStack = store_fast i (frame.f_localplus) (frame.dataStack) in
        execBytecode ({frame with dataStack = newDataStack; pc = frame.pc+1; f_localplus = newLocalPlus})
   | _ -> All.failwith "Instruction is not implemented yet"

    
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
