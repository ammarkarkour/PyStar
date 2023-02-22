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
  (* Add exception AssertionError to global names *)
  let global_names = Map.upd emptyMap "AssertionError" 
    (PYTYP (createException "AssertionError")) in
  let  virM, globalFrame = 
    makeFrame initVM code [] global_names emptyMap emptyMap in
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
  (* Add exception AssertionError to global names *)
  let global_names = Map.upd emptyMap "AssertionError" 
    (PYTYP (createException "AssertionError")) in
  let  virM, globalFrame =
    makeFrame initVM code [] global_names emptyMap emptyMap in
  let  finalVM, result = runFrame virM globalFrame in
  finalVM, result
