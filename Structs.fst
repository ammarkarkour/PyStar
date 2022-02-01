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
  | BINARY_SUBSCR: opcode
  | STORE_NAME: nat -> opcode
  | LOAD_CONST: nat -> opcode
  | LOAD_NAME: nat -> opcode
  | BUILD_TUPLE: nat -> opcode
  | BUILD_LIST: nat -> opcode
  | COMPARE_OP: nat -> opcode
  | JUMP_FORWARD: nat -> opcode
  | POP_JUMP_IF_TRUE: nat -> opcode
  | POP_JUMP_IF_FALSE: nat -> opcode
  (*| JUMP_IF_NOT_EXC_MATCH: nat -> opcode*)
  | JUMP_IF_TRUE_OR_POP: nat -> opcode
  | JUMP_IF_FALSE_OR_POP: nat -> opcode
  | JUMP_ABSOLUTE: nat -> opcode
  | LOAD_GLOBAL: nat -> opcode
  | LOAD_FAST: nat -> opcode
  | STORE_FAST: nat -> opcode
  | RETURN_VALUE: opcode
  | CALL_FUNCTION: nat -> opcode
  | MAKE_FUNCTION: nat -> opcode

noeq type bytecode = 
  | CODE: l: list opcode -> bytecode

(* Block object *)
noeq type blockObj = {
  (* currently executed opcode (type is int in cpyhton) *)
  b_type: opcode;
  (* The number of items on the value stack of the frame *)
  b_level: nat;
  (* The index of the instruction after the block *)
  b_handler: nat
}

(* The types of data allowed in datastack*)
noeq type pyObj =
  | INT: int -> pyObj
  (*| FLOAT: FStar.Real.real -> pyObj*)
  | STRING: string -> pyObj
  | BOOL: bool -> pyObj
  | LIST: list pyObj -> pyObj
  | TUPLE: list pyObj -> pyObj
  (*| DICT: Map.t pyObj pyObj -> pyObj*)  
  | NONE
  | ERR: string -> pyObj
  | FUNCTION: functionObj -> pyObj 
  | CODEOBJECT: codeObj -> pyObj
  | FRAMEOBJECT: frameObj -> pyObj

(* Function Object *)
and functionObj = {
  func_Code: pyObj; (* CODEOBJECT *)
  func_globals: Map.t string pyObj; (* DICT *)
  func_name: pyObj; (* STRING *)
  func_closure: pyObj; (* TUPLE *)
  func_defaults: pyObj; (* TUPLE *)
  (* func_annotations: pyObj; *) (* DICT *)
  (* func_kwdefaults: pyObj *) (* DICT *)
  (*
  .
  .
  . 
  *)
} 

(* object code *)
and codeObj = {
  co_code: bytecode;
  co_consts: list pyObj; // elem at index 0 must be None
  co_varnames: list pyObj;
  co_names: list pyObj; 
  (*
  .
  .
  . 
  *)
}

(* Frame object *)
and frameObj = {
  (* Simulates a stack that handles inputs of different types *)
  dataStack: list pyObj;
  (* Simulates a stack of blocks*)
  blockStack: list blockObj;
  (* code Object that is gonna run in this frame *)
  fCode: codeObj;
  (* bytecode program counter *)
  pc: nat;
  (* for fast_load and fast_store *)
  f_localplus: list pyObj;
  (* global names *)
  f_globals: Map.t string pyObj;
  (* local names *)
  f_locals: Map.t string pyObj;
  (* built-in names *)
  (* f_builtins *)
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
  (* Input to vm *)
  code: codeObj
  (*
  .
  .
  . 
  *)
}
