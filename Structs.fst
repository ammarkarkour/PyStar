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
  | GET_ITER: opcode
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
  | BUILD_MAP: nat -> opcode
  | BUILD_CONST_KEY_MAP: nat -> opcode
  | COMPARE_OP: nat -> opcode
  | JUMP_FORWARD: nat -> opcode
  | POP_JUMP_IF_TRUE: nat -> opcode
  | POP_JUMP_IF_FALSE: nat -> opcode
  (*| JUMP_IF_NOT_EXC_MATCH: nat -> opcode*)
  | JUMP_IF_TRUE_OR_POP: nat -> opcode
  | JUMP_IF_FALSE_OR_POP: nat -> opcode
  | JUMP_ABSOLUTE: nat -> opcode
  | FOR_ITER: nat -> opcode
  | LOAD_GLOBAL: nat -> opcode
  | LOAD_FAST: nat -> opcode
  | STORE_FAST: nat -> opcode
  | RETURN_VALUE: opcode
  | CALL_FUNCTION: nat -> opcode
  | MAKE_FUNCTION: nat -> opcode

noeq type bytecode = 
  | CODE: l: list opcode -> bytecode

 type hashable =
  | INTID: int -> hashable
  | BOOLID: bool -> hashable
  | STRINGID: string -> hashable

(* =============================================================== *)
(*                Python Types embeddings in F*                    *)
(* =============================================================== *)

(* 
   - builtins are the core values of non-user defined python objects.
   - It's what gets stored in obj.value.
*)
noeq type builtins = 
  | INT: int -> builtins
  | STRING: string -> builtins
  | BOOL: bool -> builtins
  | LIST: list cls  -> builtins
  | TUPLE: list cls -> builtins
  | DICT: list (cls * cls) -> builtins
  | FUNCTION: functionObj -> builtins
  | EXCEPTION: string -> builtins
  | USERDEF
  | NONE

 (*
   - user defined and non-user defined python objects
   - Notice that any entity in python is an object
 *)
and cls = {
  name: string;
  pid: int;
  value: builtins;
  fields: Map.t string pyObj;
  methods: Map.t string pyObj
}

(*
  - Entities from the interpter's point of view
  - UNFUN & BINFUN are used for objects' builtin methods 
*)
and pyObj = 
  | PYTYP: cls -> pyObj
  | UNFUNBLT: (cls -> builtins) -> pyObj
  | UNFUNOBJ: (cls -> cls) -> pyObj
  | BINFUNBLT: (cls * cls -> builtins) -> pyObj
  | CODEOBJECT: codeObj -> pyObj
  | FRAMEOBJECT: frameObj -> pyObj
  | ERR: string -> pyObj
 

(* object code *)
and codeObj = {
  co_code: bytecode;
  co_consts: list pyObj; // elem at index 0 must be None
  co_varnames: list string;
  co_names: list string; 
}
  
(* Frame object *)
and frameObj = {
  dataStack: list pyObj;
  blockStack: list blockObj;
  fCode: codeObj;
  pc: nat;
  f_localplus: list pyObj;
  f_globals: Map.t string pyObj;
  f_locals: Map.t string pyObj;
  f_idCount: nat;
  f_usedIds: Map.t hashable nat
  (* built-in names *)
  (* f_builtins *)
}

(* Function object *)
and functionObj = {
  func_Code: pyObj; (* CODEOBJECT *)
  func_globals: Map.t string pyObj; (* DICT *)
  func_name: pyObj; (* STRING *)
  func_closure: pyObj; (* TUPLE *)
  func_defaults: pyObj; (* TUPLE *)
  (* func_annotations: pyObj; *) (* DICT *)
  (* func_kwdefaults: pyObj *) (* DICT *)
} 

(* Block object *)
and blockObj = {
  (* currently executed opcode (type is int in cpyhton) *)
  b_type: opcode;
  (* The number of items on the value stack of the frame *)
  b_level: nat;
  (* The index of the instruction after the block *)
  b_handler: nat
}
  
(* VM (thread) *)
noeq type vm = {
  callStack: list frameObj;
  code: codeObj;
  vmpid: nat;
  idCount: nat;
  usedIds: Map.t hashable nat
}
