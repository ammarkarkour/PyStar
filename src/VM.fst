module VM

(* 
 Notes:
 - For creating PIDs, check first if pid exists in the PIDs mapping.
 - Once the translation pipeline is implemented, the PIDs mapping check will happen. 
*)


(* Run code object *)
let runCode code =
  (* Init the virtual machine *)
  let initVM: vm = {
    callStack = [];
    code = code;
    vmpid = 1;
    idCount = 1;
    usedIds = idsMap
  } in
  (* Create global frame to run the byte code instructions in it *)
  let  virM, globalFrame = 
    makeFrame initVM code [] emptyMap emptyMap emptyMap in
  let  finalVM, result = runFrame virM globalFrame in
  result

let runCode_returnVM code =
  (* Init the virtual machine *)
  let initVM: vm = {
    callStack = [];
    code = code;
    vmpid = 1;
    idCount = 1;
    usedIds = idsMap
  } in
  (* Create global frame to run the byte code instructions in it *)
  let  virM, globalFrame =
    makeFrame initVM code [] emptyMap emptyMap emptyMap in
  let  finalVM, result = runFrame virM globalFrame in
  finalVM, result
