module VM
 
(* Run code object *)
let runCode code =
  (* Init the virtual machine *)
  let initVM: vm = {
    callStack = [];
    functionsEnv = []; 
    code = code
  } in
  let empty_map = Map.const_on (Set.empty) NONE in
  (* Create global frame to run the byte code instructions in it *)
  let  virM, globalFrame = makeFrame initVM code [] empty_map empty_map in
  let  finalVM, result = runFrame virM globalFrame in
  result
  
 
