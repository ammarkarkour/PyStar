module VM
 
(* Run code object *)
let runCode code =
  (* Init the virtual machine *)
  let initVM: vm = {
    callStack = [];
    code = code
  } in
  (* Create global frame to run the byte code instructions in it *)
  let  virM, globalFrame = makeFrame initVM code [] emptyMap emptyMap in
  let  finalVM, result = runFrame virM globalFrame in
  result
