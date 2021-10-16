module VM

(* imported modules *)
open Exec
(* ---------------- *)

(* Create a new frame and push it in the frame stack *)
let makeFrame virM code =
  let frame: frameObj = {dataStack = []; blockStack = []; fCode = code} in
  let newVM: vm = {virM with callStack = frame::(virM.callStack)} in
  (newVM, frame)


(*
   - Runs the code in the frame and update the VM once it's done.
   Req: frame is the top element in the callstack in virM.
*)
let runFrame virM frame =
  (* Execute *) 
  let result = execBytecode frame in
  
  (* Check if result is ERR *)
  
  (* Pop the top frame*)
  let popVM = {virM with callStack = List.tail (virM.callStack)} in
  (* Push result to the top of the caller frame (top frame in the call stack) *)
  let callerFrame = List.hd popVM.callStack in
  let newCallStack = List.tail popVM.callStack in
  let newCallerFrame = {callerFrame with dataStack =  result::callerFrame.dataStack} in
  let newVM: vm = {popVM with callStack = newCallerFrame::newCallStack} in
  (newVM, result)


(* Run code object *)
let runCode code =
  (* Init the virtual machine *)
  let initVM: vm = {
    callStack = [];
    functionsEnv = []; 
    code = code
  } in
  (* Create global frame to run the byte code instructions in it *)
  let  virM, globalFrame = makeFrame initVM code in
  let  newVM, result = runFrame virM globalFrame in
  result
