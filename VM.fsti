module VM

(* imported modules *)
open FStar.List
open FStar.Heap
(* ---------------- *)

(* bytecode instruction *)
type opcode = 
  | ADD

type bytecode = 
  | CODE: list opcode -> bytecode

(* object code *)
type codeObj = {
  code: bytecode;
  (*
  .
  .
  . 
  *)
}

(* Block object *)
type blockObj = {
  (* currently executed opcode (type is int in cpyhton) *)
  b_type: opcode;
  (* The number of items on the value stack of the frame *)
  b_level: nat;
  (* The index of the instruction after the block *)
  b_handler: nat
}


(* Frame object *)
noeq type frameObj = {
  (* Simulates a stack that handles inputs of different types *)
  dataStack: heap;
  (* Simulates a stack of blocks*)
  blockStack: list blockObj
  (*
  .
  .
  . 
  *)
}


(* Function Object *)
type functionObj = {
  funCode: bytecode
  (*
  .
  .
  . 
  *)
} 

(* VM (thread) *)
noeq type vm = {
  (* Simulates a stack of frames *)
  callStack: list frameObj;
  (* Simulates a stack of (function name, functionObj) *)
  functionsEnv: list (string * functionObj);
  (* Input to vm *)
  code: codeObj
  (*
  .
  .
  . 
  *)
}


