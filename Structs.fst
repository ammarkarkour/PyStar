module Structs

(* bytecode instruction *)
type opcode = 
  | NOP: opcode
  | POP_TOP: opcode
  | ROT_TWO: opcode
  | ROT_THREE: opcode
  | ROT_FOUR: opcode
  | DUP_TOP: opcode
  | DUP_TOP_TWO:opcode
  | UNARY_POSITIVE: opcode
  | UNARY_NEGATIVE: opcode
  | UNARY_NOT: opcode
  | BINARY_MULTIPLY: opcode
  | BINARY_FLOOR_DIVIDE: opcode
  | BINARY_MODULO: opcode
  | BINARY_ADD: opcode
  | BINARY_SUBTRACT: opcode
  | LOAD_CONST: nat -> opcode
  | LOAD_FAST: nat -> opcode
  | STORE_FAST: nat -> opcode
  | RETURN_VALUE: opcode

type bytecode = 
  | CODE: l: list opcode -> bytecode

(* The types of data allowed in datastack*)
noeq type pyObj =
  | INT: int -> pyObj
  (*| FLOAT: FStar.Real.real -> pyObj*)
  | STRING: string -> pyObj
  | BOOL: bool -> pyObj
  | NONE
  | ERR: string -> pyObj

(* object code *)
noeq type codeObj = {
  co_code: bytecode;
  co_consts: list pyObj; // elem at index 0 must be None
  co_varnames: list pyObj
  (*
  .
  .
  . 
  *)
}

(* Block object *)
noeq type blockObj = {
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
  dataStack: list pyObj;
  (* Simulates a stack of blocks*)
  blockStack: list blockObj;
  (* code Object that is gonna run in this frame *)
  fCode: codeObj
  (*
  .
  .
  . 
  *)
}

(* Function Object *)
noeq type functionObj = {
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
