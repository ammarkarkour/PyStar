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
  
(* 
   - The main purpose of using pyObj is to trick 
     the typing system into accepting the type wihtout
     satisfying the positivity condition.

   - The main reason of having X as constructor is to
     be ablt to create empty mappings.

   - Any type/object that is python accessable is an object
     of the record type0 wraped inside the TYP constructor.
     Some functions are provided to create objects of
     the builtin types. int, string, list, tuple    
   - Types that are jsut interprter representations, are 
     also included. eg: CODEobj and Frameobj.
*)
noeq type builtins = 
  | INT: int -> builtins
  | STRING: string -> builtins
  | BOOL: bool -> builtins
  | LIST: list pyObj  -> builtins
  | TUPLE: list pyObj -> builtins  
  | NONE
  
and pyObj = 
  | TYP: type0 -> pyObj
  | CODEOBJECT: codeObj -> pyObj
  | FRAMEOBJECT: frameObj -> pyObj
  |  ERR: string -> pyObj
  
and type0 = {
  name: string;
  pid: int;
  value: builtins;
  fields: Map.t string pyObj;
  methods: Map.t string pyObj
} 

(* object code *)
and codeObj = {
  co_code: bytecode;
  co_consts: list pyObj; // elem at index 0 must be None
  co_varnames: list pyObj;
  co_names: list pyObj; 
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
  (* built-in names *)
  (* f_builtins *)
}

private let emptyMap: Map.t string pyObj  = Map.const_on (Set.empty) (ERR("UNDEFINED"))

(* *)
let createInt (v:int) = 
  let obj: type0= {
    name = "int";
    pid = 0;
    value = INT(v);
    fields = emptyMap;
    methods = emptyMap
  } in
  TYP(obj)

(* VM (thread) *)
noeq type vm = {
  callStack: list frameObj;
  code: codeObj
}

let five = createInt 5

let r:builtins =
  match five with
  | TYP(c) -> c.value
  | _ -> NONE
 



(* --------------------------------------------------------------------------- *)




// let emptyMap = Map.const_on (Set.empty) NONE

// type int0 = {id="int"; fields = emptyMap; methods=emptyMap}

// let r = {id="int"; fields = emptyMap; methods=emptyMap}

// let u = int0.id
// let y = r.id

// let int0: pyObj = 
//   let obj:type0 = {id="int"; fields = emptyMap; methods=emptyMap} in
//   INT(obj)


// (* The types of data allowed in datastack*)
// noeq type pyObj =
//   | INT: int -> pyObj
//   (*| FLOAT: FStar.Real.real -> pyObj*)
//   | STRING: string -> pyObj
//   | BOOL: bool -> pyObj
//   | LIST: list pyObj -> pyObj
//   | TUPLE: list pyObj -> pyObj
//   (*| DICT: Map.t pyObj pyObj -> pyObj*)  
//   | NONE
//   | ERR: string -> pyObj
//   | FUNCTION: functionObj -> pyObj 
//   | CODEOBJECT: codeObj -> pyObj
//   | FRAMEOBJECT: frameObj -> pyObj

// (* Function Object *)
// and functionObj = {
//   func_Code: pyObj; (* CODEOBJECT *)
//   func_globals: Map.t string pyObj; (* DICT *)
//   func_name: pyObj; (* STRING *)
//   func_closure: pyObj; (* TUPLE *)
//   func_defaults: pyObj; (* TUPLE *)
//   (* func_annotations: pyObj; *) (* DICT *)
//   (* func_kwdefaults: pyObj *) (* DICT *)
//   (*
//   .
//   .
//   . 
//   *)
// } 

// (* object code *)
// and codeObj = {
//   co_code: bytecode;
//   co_consts: list pyObj; // elem at index 0 must be None
//   co_varnames: list pyObj;
//   co_names: list pyObj; 
//   (*
//   .
//   .
//   . 
//   *)
// }

// (* Frame object *)
// and frameObj = {
//   (* Simulates a stack that handles inputs of different types *)
//   dataStack: list pyObj;
//   (* Simulates a stack of blocks*)
//   blockStack: list blockObj;
//   (* code Object that is gonna run in this frame *)
//   fCode: codeObj;
//   (* bytecode program counter *)
//   pc: nat;
//   (* for fast_load and fast_store *)
//   f_localplus: list pyObj;
//   (* global names *)
//   f_globals: Map.t string pyObj;
//   (* local names *)
//   f_locals: Map.t string pyObj;
//   (* built-in names *)
//   (* f_builtins *)
//   (*
//   .
//   .
//   . 
//   *)
// }


// (* VM (thread) *)
// noeq type vm = {
//   (* Simulates a stack of frames *)
//   callStack: list frameObj;
//   (* Input to vm *)
//   code: codeObj
//   (*
//   .
//   .
//   . 
//   *)
//}
