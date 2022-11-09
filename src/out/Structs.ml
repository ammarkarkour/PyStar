open Prims
type opcode =
  | NOP 
  | POP_TOP 
  | ROT_TWO 
  | ROT_THREE 
  | ROT_FOUR 
  | DUP_TOP 
  | DUP_TOP_TWO 
  | UNARY_POSITIVE 
  | UNARY_NEGATIVE 
  | UNARY_NOT 
  | GET_ITER 
  | BINARY_MULTIPLY 
  | BINARY_FLOOR_DIVIDE 
  | BINARY_MODULO 
  | BINARY_ADD 
  | BINARY_SUBTRACT 
  | BINARY_SUBSCR 
  | INPLACE_MULTIPLY 
  | INPLACE_FLOOR_DIVIDE 
  | INPLACE_MODULO 
  | INPLACE_ADD 
  | INPLACE_SUBTRACT 
  | STORE_NAME of Prims.nat 
  | LOAD_CONST of Prims.nat 
  | LOAD_NAME of Prims.nat 
  | BUILD_TUPLE of Prims.nat 
  | BUILD_LIST of Prims.nat 
  | BUILD_MAP of Prims.nat 
  | BUILD_CONST_KEY_MAP of Prims.nat 
  | COMPARE_OP of Prims.nat 
  | JUMP_FORWARD of Prims.nat 
  | POP_JUMP_IF_TRUE of Prims.nat 
  | POP_JUMP_IF_FALSE of Prims.nat 
  | JUMP_IF_TRUE_OR_POP of Prims.nat 
  | JUMP_IF_FALSE_OR_POP of Prims.nat 
  | JUMP_ABSOLUTE of Prims.nat 
  | FOR_ITER of Prims.nat 
  | LOAD_GLOBAL of Prims.nat 
  | LOAD_FAST of Prims.nat 
  | STORE_FAST of Prims.nat 
  | LOAD_CLOSURE of Prims.nat 
  | LOAD_DEREF of Prims.nat 
  | STORE_DEREF of Prims.nat 
  | RETURN_VALUE 
  | CALL_FUNCTION of Prims.nat 
  | MAKE_FUNCTION of Prims.nat 
  | BUILD_SLICE of Prims.nat 
let (uu___is_NOP : opcode -> Prims.bool) =
  fun projectee -> match projectee with | NOP -> true | uu___ -> false
let (uu___is_POP_TOP : opcode -> Prims.bool) =
  fun projectee -> match projectee with | POP_TOP -> true | uu___ -> false
let (uu___is_ROT_TWO : opcode -> Prims.bool) =
  fun projectee -> match projectee with | ROT_TWO -> true | uu___ -> false
let (uu___is_ROT_THREE : opcode -> Prims.bool) =
  fun projectee -> match projectee with | ROT_THREE -> true | uu___ -> false
let (uu___is_ROT_FOUR : opcode -> Prims.bool) =
  fun projectee -> match projectee with | ROT_FOUR -> true | uu___ -> false
let (uu___is_DUP_TOP : opcode -> Prims.bool) =
  fun projectee -> match projectee with | DUP_TOP -> true | uu___ -> false
let (uu___is_DUP_TOP_TWO : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | DUP_TOP_TWO -> true | uu___ -> false
let (uu___is_UNARY_POSITIVE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | UNARY_POSITIVE -> true | uu___ -> false
let (uu___is_UNARY_NEGATIVE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | UNARY_NEGATIVE -> true | uu___ -> false
let (uu___is_UNARY_NOT : opcode -> Prims.bool) =
  fun projectee -> match projectee with | UNARY_NOT -> true | uu___ -> false
let (uu___is_GET_ITER : opcode -> Prims.bool) =
  fun projectee -> match projectee with | GET_ITER -> true | uu___ -> false
let (uu___is_BINARY_MULTIPLY : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BINARY_MULTIPLY -> true | uu___ -> false
let (uu___is_BINARY_FLOOR_DIVIDE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BINARY_FLOOR_DIVIDE -> true | uu___ -> false
let (uu___is_BINARY_MODULO : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BINARY_MODULO -> true | uu___ -> false
let (uu___is_BINARY_ADD : opcode -> Prims.bool) =
  fun projectee -> match projectee with | BINARY_ADD -> true | uu___ -> false
let (uu___is_BINARY_SUBTRACT : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BINARY_SUBTRACT -> true | uu___ -> false
let (uu___is_BINARY_SUBSCR : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BINARY_SUBSCR -> true | uu___ -> false
let (uu___is_INPLACE_MULTIPLY : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | INPLACE_MULTIPLY -> true | uu___ -> false
let (uu___is_INPLACE_FLOOR_DIVIDE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | INPLACE_FLOOR_DIVIDE -> true | uu___ -> false
let (uu___is_INPLACE_MODULO : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | INPLACE_MODULO -> true | uu___ -> false
let (uu___is_INPLACE_ADD : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | INPLACE_ADD -> true | uu___ -> false
let (uu___is_INPLACE_SUBTRACT : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | INPLACE_SUBTRACT -> true | uu___ -> false
let (uu___is_STORE_NAME : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | STORE_NAME _0 -> true | uu___ -> false
let (__proj__STORE_NAME__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | STORE_NAME _0 -> _0
let (uu___is_LOAD_CONST : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_CONST _0 -> true | uu___ -> false
let (__proj__LOAD_CONST__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_CONST _0 -> _0
let (uu___is_LOAD_NAME : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_NAME _0 -> true | uu___ -> false
let (__proj__LOAD_NAME__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_NAME _0 -> _0
let (uu___is_BUILD_TUPLE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BUILD_TUPLE _0 -> true | uu___ -> false
let (__proj__BUILD_TUPLE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | BUILD_TUPLE _0 -> _0
let (uu___is_BUILD_LIST : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BUILD_LIST _0 -> true | uu___ -> false
let (__proj__BUILD_LIST__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | BUILD_LIST _0 -> _0
let (uu___is_BUILD_MAP : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BUILD_MAP _0 -> true | uu___ -> false
let (__proj__BUILD_MAP__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | BUILD_MAP _0 -> _0
let (uu___is_BUILD_CONST_KEY_MAP : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BUILD_CONST_KEY_MAP _0 -> true | uu___ -> false
let (__proj__BUILD_CONST_KEY_MAP__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | BUILD_CONST_KEY_MAP _0 -> _0
let (uu___is_COMPARE_OP : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | COMPARE_OP _0 -> true | uu___ -> false
let (__proj__COMPARE_OP__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | COMPARE_OP _0 -> _0
let (uu___is_JUMP_FORWARD : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | JUMP_FORWARD _0 -> true | uu___ -> false
let (__proj__JUMP_FORWARD__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | JUMP_FORWARD _0 -> _0
let (uu___is_POP_JUMP_IF_TRUE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | POP_JUMP_IF_TRUE _0 -> true | uu___ -> false
let (__proj__POP_JUMP_IF_TRUE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | POP_JUMP_IF_TRUE _0 -> _0
let (uu___is_POP_JUMP_IF_FALSE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | POP_JUMP_IF_FALSE _0 -> true | uu___ -> false
let (__proj__POP_JUMP_IF_FALSE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | POP_JUMP_IF_FALSE _0 -> _0
let (uu___is_JUMP_IF_TRUE_OR_POP : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | JUMP_IF_TRUE_OR_POP _0 -> true | uu___ -> false
let (__proj__JUMP_IF_TRUE_OR_POP__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | JUMP_IF_TRUE_OR_POP _0 -> _0
let (uu___is_JUMP_IF_FALSE_OR_POP : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | JUMP_IF_FALSE_OR_POP _0 -> true | uu___ -> false
let (__proj__JUMP_IF_FALSE_OR_POP__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | JUMP_IF_FALSE_OR_POP _0 -> _0
let (uu___is_JUMP_ABSOLUTE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | JUMP_ABSOLUTE _0 -> true | uu___ -> false
let (__proj__JUMP_ABSOLUTE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | JUMP_ABSOLUTE _0 -> _0
let (uu___is_FOR_ITER : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | FOR_ITER _0 -> true | uu___ -> false
let (__proj__FOR_ITER__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | FOR_ITER _0 -> _0
let (uu___is_LOAD_GLOBAL : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_GLOBAL _0 -> true | uu___ -> false
let (__proj__LOAD_GLOBAL__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_GLOBAL _0 -> _0
let (uu___is_LOAD_FAST : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_FAST _0 -> true | uu___ -> false
let (__proj__LOAD_FAST__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_FAST _0 -> _0
let (uu___is_STORE_FAST : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | STORE_FAST _0 -> true | uu___ -> false
let (__proj__STORE_FAST__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | STORE_FAST _0 -> _0
let (uu___is_LOAD_CLOSURE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_CLOSURE _0 -> true | uu___ -> false
let (__proj__LOAD_CLOSURE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_CLOSURE _0 -> _0
let (uu___is_LOAD_DEREF : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | LOAD_DEREF _0 -> true | uu___ -> false
let (__proj__LOAD_DEREF__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | LOAD_DEREF _0 -> _0
let (uu___is_STORE_DEREF : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | STORE_DEREF _0 -> true | uu___ -> false
let (__proj__STORE_DEREF__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | STORE_DEREF _0 -> _0
let (uu___is_RETURN_VALUE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | RETURN_VALUE -> true | uu___ -> false
let (uu___is_CALL_FUNCTION : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | CALL_FUNCTION _0 -> true | uu___ -> false
let (__proj__CALL_FUNCTION__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | CALL_FUNCTION _0 -> _0
let (uu___is_MAKE_FUNCTION : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | MAKE_FUNCTION _0 -> true | uu___ -> false
let (__proj__MAKE_FUNCTION__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | MAKE_FUNCTION _0 -> _0
let (uu___is_BUILD_SLICE : opcode -> Prims.bool) =
  fun projectee ->
    match projectee with | BUILD_SLICE _0 -> true | uu___ -> false
let (__proj__BUILD_SLICE__item___0 : opcode -> Prims.nat) =
  fun projectee -> match projectee with | BUILD_SLICE _0 -> _0
type bytecode =
  | CODE of opcode Prims.list 
let (uu___is_CODE : bytecode -> Prims.bool) = fun projectee -> true
let (__proj__CODE__item__l : bytecode -> opcode Prims.list) =
  fun projectee -> match projectee with | CODE l -> l
type hashable =
  | INTID of Prims.int 
  | BOOLID of Prims.bool 
  | STRINGID of Prims.string 
let (uu___is_INTID : hashable -> Prims.bool) =
  fun projectee -> match projectee with | INTID _0 -> true | uu___ -> false
let (__proj__INTID__item___0 : hashable -> Prims.int) =
  fun projectee -> match projectee with | INTID _0 -> _0
let (uu___is_BOOLID : hashable -> Prims.bool) =
  fun projectee -> match projectee with | BOOLID _0 -> true | uu___ -> false
let (__proj__BOOLID__item___0 : hashable -> Prims.bool) =
  fun projectee -> match projectee with | BOOLID _0 -> _0
let (uu___is_STRINGID : hashable -> Prims.bool) =
  fun projectee ->
    match projectee with | STRINGID _0 -> true | uu___ -> false
let (__proj__STRINGID__item___0 : hashable -> Prims.string) =
  fun projectee -> match projectee with | STRINGID _0 -> _0
type builtins =
  | INT of Prims.int 
  | STRING of Prims.string 
  | BOOL of Prims.bool 
  | LIST of cls Prims.list 
  | TUPLE of cls Prims.list 
  | DICT of (cls * cls) Prims.list 
  | FUNCTION of functionObj 
  | EXCEPTION of Prims.string 
  | SLICE of Prims.int FStar_Pervasives_Native.option * Prims.int
  FStar_Pervasives_Native.option * Prims.int FStar_Pervasives_Native.option 
  | USERDEF 
  | NONE 
and cls =
  {
  name: Prims.string ;
  pid: Prims.int ;
  value: builtins ;
  fields: (Prims.string, pyObj) FStar_Map.t ;
  methods: (Prims.string, pyObj) FStar_Map.t }
and pyObj =
  | PYTYP of cls 
  | UNFUNBLT of (cls -> builtins) 
  | UNFUNOBJ of (cls -> cls) 
  | BINFUNBLT of ((cls * cls) -> builtins) 
  | CODEOBJECT of codeObj 
  | FRAMEOBJECT of frameObj 
  | ERR of Prims.string 
and codeObj =
  {
  co_code: bytecode ;
  co_consts: pyObj Prims.list ;
  co_varnames: Prims.string Prims.list ;
  co_names: Prims.string Prims.list ;
  co_cellvars: Prims.string Prims.list ;
  co_freevars: Prims.string Prims.list }
and frameObj =
  {
  dataStack: pyObj Prims.list ;
  blockStack: blockObj Prims.list ;
  fCode: codeObj ;
  pc: Prims.nat ;
  f_localplus: pyObj Prims.list ;
  f_globals: (Prims.string, pyObj) FStar_Map.t ;
  f_locals: (Prims.string, pyObj) FStar_Map.t ;
  f_cells: (Prims.string, pyObj) FStar_Map.t ;
  f_idCount: Prims.nat ;
  f_usedIds: (hashable, Prims.nat) FStar_Map.t }
and functionObj =
  {
  func_Code: pyObj ;
  func_globals: (Prims.string, pyObj) FStar_Map.t ;
  func_cells: (Prims.string, pyObj) FStar_Map.t ;
  func_name: pyObj ;
  func_closure: pyObj ;
  func_defaults: pyObj }
and blockObj = {
  b_type: opcode ;
  b_level: Prims.nat ;
  b_handler: Prims.nat }
let (uu___is_INT : builtins -> Prims.bool) =
  fun projectee -> match projectee with | INT _0 -> true | uu___ -> false
let (__proj__INT__item___0 : builtins -> Prims.int) =
  fun projectee -> match projectee with | INT _0 -> _0
let (uu___is_STRING : builtins -> Prims.bool) =
  fun projectee -> match projectee with | STRING _0 -> true | uu___ -> false
let (__proj__STRING__item___0 : builtins -> Prims.string) =
  fun projectee -> match projectee with | STRING _0 -> _0
let (uu___is_BOOL : builtins -> Prims.bool) =
  fun projectee -> match projectee with | BOOL _0 -> true | uu___ -> false
let (__proj__BOOL__item___0 : builtins -> Prims.bool) =
  fun projectee -> match projectee with | BOOL _0 -> _0
let (uu___is_LIST : builtins -> Prims.bool) =
  fun projectee -> match projectee with | LIST _0 -> true | uu___ -> false
let (__proj__LIST__item___0 : builtins -> cls Prims.list) =
  fun projectee -> match projectee with | LIST _0 -> _0
let (uu___is_TUPLE : builtins -> Prims.bool) =
  fun projectee -> match projectee with | TUPLE _0 -> true | uu___ -> false
let (__proj__TUPLE__item___0 : builtins -> cls Prims.list) =
  fun projectee -> match projectee with | TUPLE _0 -> _0
let (uu___is_DICT : builtins -> Prims.bool) =
  fun projectee -> match projectee with | DICT _0 -> true | uu___ -> false
let (__proj__DICT__item___0 : builtins -> (cls * cls) Prims.list) =
  fun projectee -> match projectee with | DICT _0 -> _0
let (uu___is_FUNCTION : builtins -> Prims.bool) =
  fun projectee ->
    match projectee with | FUNCTION _0 -> true | uu___ -> false
let (__proj__FUNCTION__item___0 : builtins -> functionObj) =
  fun projectee -> match projectee with | FUNCTION _0 -> _0
let (uu___is_EXCEPTION : builtins -> Prims.bool) =
  fun projectee ->
    match projectee with | EXCEPTION _0 -> true | uu___ -> false
let (__proj__EXCEPTION__item___0 : builtins -> Prims.string) =
  fun projectee -> match projectee with | EXCEPTION _0 -> _0
let (uu___is_SLICE : builtins -> Prims.bool) =
  fun projectee ->
    match projectee with | SLICE (_0, _1, _2) -> true | uu___ -> false
let (__proj__SLICE__item___0 :
  builtins -> Prims.int FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | SLICE (_0, _1, _2) -> _0
let (__proj__SLICE__item___1 :
  builtins -> Prims.int FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | SLICE (_0, _1, _2) -> _1
let (__proj__SLICE__item___2 :
  builtins -> Prims.int FStar_Pervasives_Native.option) =
  fun projectee -> match projectee with | SLICE (_0, _1, _2) -> _2
let (uu___is_USERDEF : builtins -> Prims.bool) =
  fun projectee -> match projectee with | USERDEF -> true | uu___ -> false
let (uu___is_NONE : builtins -> Prims.bool) =
  fun projectee -> match projectee with | NONE -> true | uu___ -> false
let (__proj__Mkcls__item__name : cls -> Prims.string) =
  fun projectee ->
    match projectee with | { name; pid; value; fields; methods;_} -> name
let (__proj__Mkcls__item__pid : cls -> Prims.int) =
  fun projectee ->
    match projectee with | { name; pid; value; fields; methods;_} -> pid
let (__proj__Mkcls__item__value : cls -> builtins) =
  fun projectee ->
    match projectee with | { name; pid; value; fields; methods;_} -> value
let (__proj__Mkcls__item__fields : cls -> (Prims.string, pyObj) FStar_Map.t)
  =
  fun projectee ->
    match projectee with | { name; pid; value; fields; methods;_} -> fields
let (__proj__Mkcls__item__methods : cls -> (Prims.string, pyObj) FStar_Map.t)
  =
  fun projectee ->
    match projectee with | { name; pid; value; fields; methods;_} -> methods
let (uu___is_PYTYP : pyObj -> Prims.bool) =
  fun projectee -> match projectee with | PYTYP _0 -> true | uu___ -> false
let (__proj__PYTYP__item___0 : pyObj -> cls) =
  fun projectee -> match projectee with | PYTYP _0 -> _0
let (uu___is_UNFUNBLT : pyObj -> Prims.bool) =
  fun projectee ->
    match projectee with | UNFUNBLT _0 -> true | uu___ -> false
let (__proj__UNFUNBLT__item___0 : pyObj -> cls -> builtins) =
  fun projectee -> match projectee with | UNFUNBLT _0 -> _0
let (uu___is_UNFUNOBJ : pyObj -> Prims.bool) =
  fun projectee ->
    match projectee with | UNFUNOBJ _0 -> true | uu___ -> false
let (__proj__UNFUNOBJ__item___0 : pyObj -> cls -> cls) =
  fun projectee -> match projectee with | UNFUNOBJ _0 -> _0
let (uu___is_BINFUNBLT : pyObj -> Prims.bool) =
  fun projectee ->
    match projectee with | BINFUNBLT _0 -> true | uu___ -> false
let (__proj__BINFUNBLT__item___0 : pyObj -> (cls * cls) -> builtins) =
  fun projectee -> match projectee with | BINFUNBLT _0 -> _0
let (uu___is_CODEOBJECT : pyObj -> Prims.bool) =
  fun projectee ->
    match projectee with | CODEOBJECT _0 -> true | uu___ -> false
let (__proj__CODEOBJECT__item___0 : pyObj -> codeObj) =
  fun projectee -> match projectee with | CODEOBJECT _0 -> _0
let (uu___is_FRAMEOBJECT : pyObj -> Prims.bool) =
  fun projectee ->
    match projectee with | FRAMEOBJECT _0 -> true | uu___ -> false
let (__proj__FRAMEOBJECT__item___0 : pyObj -> frameObj) =
  fun projectee -> match projectee with | FRAMEOBJECT _0 -> _0
let (uu___is_ERR : pyObj -> Prims.bool) =
  fun projectee -> match projectee with | ERR _0 -> true | uu___ -> false
let (__proj__ERR__item___0 : pyObj -> Prims.string) =
  fun projectee -> match projectee with | ERR _0 -> _0
let (__proj__MkcodeObj__item__co_code : codeObj -> bytecode) =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_code
let (__proj__MkcodeObj__item__co_consts : codeObj -> pyObj Prims.list) =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_consts
let (__proj__MkcodeObj__item__co_varnames :
  codeObj -> Prims.string Prims.list) =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_varnames
let (__proj__MkcodeObj__item__co_names : codeObj -> Prims.string Prims.list)
  =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_names
let (__proj__MkcodeObj__item__co_cellvars :
  codeObj -> Prims.string Prims.list) =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_cellvars
let (__proj__MkcodeObj__item__co_freevars :
  codeObj -> Prims.string Prims.list) =
  fun projectee ->
    match projectee with
    | { co_code; co_consts; co_varnames; co_names; co_cellvars;
        co_freevars;_} -> co_freevars
let (__proj__MkframeObj__item__dataStack : frameObj -> pyObj Prims.list) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> dataStack
let (__proj__MkframeObj__item__blockStack : frameObj -> blockObj Prims.list)
  =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> blockStack
let (__proj__MkframeObj__item__fCode : frameObj -> codeObj) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> fCode
let (__proj__MkframeObj__item__pc : frameObj -> Prims.nat) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> pc
let (__proj__MkframeObj__item__f_localplus : frameObj -> pyObj Prims.list) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_localplus
let (__proj__MkframeObj__item__f_globals :
  frameObj -> (Prims.string, pyObj) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_globals
let (__proj__MkframeObj__item__f_locals :
  frameObj -> (Prims.string, pyObj) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_locals
let (__proj__MkframeObj__item__f_cells :
  frameObj -> (Prims.string, pyObj) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_cells
let (__proj__MkframeObj__item__f_idCount : frameObj -> Prims.nat) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_idCount
let (__proj__MkframeObj__item__f_usedIds :
  frameObj -> (hashable, Prims.nat) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { dataStack; blockStack; fCode; pc; f_localplus; f_globals; f_locals;
        f_cells; f_idCount; f_usedIds;_} -> f_usedIds
let (__proj__MkfunctionObj__item__func_Code : functionObj -> pyObj) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_Code
let (__proj__MkfunctionObj__item__func_globals :
  functionObj -> (Prims.string, pyObj) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_globals
let (__proj__MkfunctionObj__item__func_cells :
  functionObj -> (Prims.string, pyObj) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_cells
let (__proj__MkfunctionObj__item__func_name : functionObj -> pyObj) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_name
let (__proj__MkfunctionObj__item__func_closure : functionObj -> pyObj) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_closure
let (__proj__MkfunctionObj__item__func_defaults : functionObj -> pyObj) =
  fun projectee ->
    match projectee with
    | { func_Code; func_globals; func_cells; func_name; func_closure;
        func_defaults;_} -> func_defaults
let (__proj__MkblockObj__item__b_type : blockObj -> opcode) =
  fun projectee ->
    match projectee with | { b_type; b_level; b_handler;_} -> b_type
let (__proj__MkblockObj__item__b_level : blockObj -> Prims.nat) =
  fun projectee ->
    match projectee with | { b_type; b_level; b_handler;_} -> b_level
let (__proj__MkblockObj__item__b_handler : blockObj -> Prims.nat) =
  fun projectee ->
    match projectee with | { b_type; b_level; b_handler;_} -> b_handler
type vm =
  {
  callStack: frameObj Prims.list ;
  code: codeObj ;
  vmpid: Prims.nat ;
  idCount: Prims.nat ;
  usedIds: (hashable, Prims.nat) FStar_Map.t }
let (__proj__Mkvm__item__callStack : vm -> frameObj Prims.list) =
  fun projectee ->
    match projectee with
    | { callStack; code; vmpid; idCount; usedIds;_} -> callStack
let (__proj__Mkvm__item__code : vm -> codeObj) =
  fun projectee ->
    match projectee with
    | { callStack; code; vmpid; idCount; usedIds;_} -> code
let (__proj__Mkvm__item__vmpid : vm -> Prims.nat) =
  fun projectee ->
    match projectee with
    | { callStack; code; vmpid; idCount; usedIds;_} -> vmpid
let (__proj__Mkvm__item__idCount : vm -> Prims.nat) =
  fun projectee ->
    match projectee with
    | { callStack; code; vmpid; idCount; usedIds;_} -> idCount
let (__proj__Mkvm__item__usedIds : vm -> (hashable, Prims.nat) FStar_Map.t) =
  fun projectee ->
    match projectee with
    | { callStack; code; vmpid; idCount; usedIds;_} -> usedIds