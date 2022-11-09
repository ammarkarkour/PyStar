open Prims
let (bc_0x7f1fd820d2f0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CLOSURE Prims.int_one;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (6));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_DEREF Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (7));
    Structs.LOAD_CONST (Prims.of_int (8));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_DEREF Prims.int_zero;
    Structs.LOAD_CLOSURE Prims.int_zero;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (9));
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (11));
    Structs.LOAD_CONST (Prims.of_int (12));
    Structs.LOAD_CONST (Prims.of_int (13));
    Structs.CALL_FUNCTION (Prims.of_int (3));
    Structs.LOAD_DEREF Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (14));
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.BUILD_LIST (Prims.of_int (4));
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.RETURN_VALUE]
let (bc_0x7f1fd820d240 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_DEREF Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7f1fd820d240 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ())]
let varnames_0x7f1fd820d240 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f1fd820d240 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f1fd820d240 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7f1fd820d240 : Prims.string Prims.list) =
  ["test_function_to_be_called"]
let (co_0x7f1fd820d240 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd820d240;
    Structs.co_consts = consts_0x7f1fd820d240;
    Structs.co_varnames = (varnames_0x7f1fd820d240 ());
    Structs.co_names = (names_0x7f1fd820d240 ());
    Structs.co_cellvars = (cellvars_0x7f1fd820d240 ());
    Structs.co_freevars = freevars_0x7f1fd820d240
  }
let (bc_0x7f1fd820d190 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f1fd820d190 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let varnames_0x7f1fd820d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f1fd820d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f1fd820d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f1fd820d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f1fd820d190 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd820d190;
    Structs.co_consts = consts_0x7f1fd820d190;
    Structs.co_varnames = (varnames_0x7f1fd820d190 ());
    Structs.co_names = (names_0x7f1fd820d190 ());
    Structs.co_cellvars = (cellvars_0x7f1fd820d190 ());
    Structs.co_freevars = (freevars_0x7f1fd820d190 ())
  }
let (bc_0x7f1fd820d0e0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_one;
    Structs.COMPARE_OP Prims.int_one;
    Structs.POP_JUMP_IF_FALSE (Prims.of_int (12));
    Structs.LOAD_CONST Prims.int_one;
    Structs.RETURN_VALUE;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_DEREF Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.BINARY_SUBTRACT;
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.BINARY_ADD;
    Structs.RETURN_VALUE]
let (consts_0x7f1fd820d0e0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let (varnames_0x7f1fd820d0e0 : Prims.string Prims.list) = ["x"]
let names_0x7f1fd820d0e0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f1fd820d0e0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7f1fd820d0e0 : Prims.string Prims.list) = ["test_recursive"]
let (co_0x7f1fd820d0e0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd820d0e0;
    Structs.co_consts = consts_0x7f1fd820d0e0;
    Structs.co_varnames = varnames_0x7f1fd820d0e0;
    Structs.co_names = (names_0x7f1fd820d0e0 ());
    Structs.co_cellvars = (cellvars_0x7f1fd820d0e0 ());
    Structs.co_freevars = freevars_0x7f1fd820d0e0
  }
let (bc_0x7f1fd8200f50 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.BUILD_TUPLE (Prims.of_int (3));
    Structs.RETURN_VALUE]
let (consts_0x7f1fd8200f50 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ())]
let (varnames_0x7f1fd8200f50 : Prims.string Prims.list) = ["x"; "y"; "z"]
let names_0x7f1fd8200f50 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f1fd8200f50 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f1fd8200f50 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f1fd8200f50 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd8200f50;
    Structs.co_consts = consts_0x7f1fd8200f50;
    Structs.co_varnames = varnames_0x7f1fd8200f50;
    Structs.co_names = (names_0x7f1fd8200f50 ());
    Structs.co_cellvars = (cellvars_0x7f1fd8200f50 ());
    Structs.co_freevars = (freevars_0x7f1fd8200f50 ())
  }
let (bc_0x7f1fd8200df0 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f1fd8200df0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let varnames_0x7f1fd8200df0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f1fd8200df0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f1fd8200df0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f1fd8200df0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f1fd8200df0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd8200df0;
    Structs.co_consts = consts_0x7f1fd8200df0;
    Structs.co_varnames = (varnames_0x7f1fd8200df0 ());
    Structs.co_names = (names_0x7f1fd8200df0 ());
    Structs.co_cellvars = (cellvars_0x7f1fd8200df0 ());
    Structs.co_freevars = (freevars_0x7f1fd8200df0 ())
  }
let (consts_0x7f1fd820d2f0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP
     (PyString.createString "Tests for calling/defining functions\n    ");
  Structs.CODEOBJECT co_0x7f1fd8200df0;
  Structs.PYTYP
    (PyString.createString
       "top_level.<locals>.test_function_def_with_no_params");
  Structs.CODEOBJECT co_0x7f1fd8200f50;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_function_def_with_params");
  Structs.CODEOBJECT co_0x7f1fd820d0e0;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test_recursive");
  Structs.CODEOBJECT co_0x7f1fd820d190;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_function_to_be_called");
  Structs.CODEOBJECT co_0x7f1fd820d240;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test_closure");
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (5)))]
let (varnames_0x7f1fd820d2f0 : Prims.string Prims.list) =
  ["test_function_def_with_no_params";
  "test_function_def_with_params";
  "test_closure";
  "result"]
let names_0x7f1fd820d2f0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (cellvars_0x7f1fd820d2f0 : Prims.string Prims.list) =
  ["test_function_to_be_called"; "test_recursive"]
let freevars_0x7f1fd820d2f0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f1fd820d2f0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f1fd820d2f0;
    Structs.co_consts = consts_0x7f1fd820d2f0;
    Structs.co_varnames = varnames_0x7f1fd820d2f0;
    Structs.co_names = (names_0x7f1fd820d2f0 ());
    Structs.co_cellvars = cellvars_0x7f1fd820d2f0;
    Structs.co_freevars = (freevars_0x7f1fd820d2f0 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f1fd820d2f0
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___