open Prims
let (bc_0x7fb3abf9d500 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_DEREF Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CLOSURE Prims.int_zero;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (6));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CLOSURE Prims.int_zero;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (7));
    Structs.LOAD_CONST (Prims.of_int (8));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_CLOSURE Prims.int_zero;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (9));
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_CLOSURE Prims.int_zero;
    Structs.BUILD_TUPLE Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (11));
    Structs.LOAD_CONST (Prims.of_int (12));
    Structs.MAKE_FUNCTION (Prims.of_int (8));
    Structs.STORE_FAST (Prims.of_int (4));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST (Prims.of_int (4));
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.BUILD_LIST (Prims.of_int (5));
    Structs.STORE_FAST (Prims.of_int (5));
    Structs.LOAD_FAST (Prims.of_int (5));
    Structs.RETURN_VALUE]
let (bc_0x7fb3abf9d450 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (6));
    Structs.BUILD_CONST_KEY_MAP (Prims.of_int (5));
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_DEREF Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d450 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (4)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (6)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (8)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (10)));
  Structs.PYTYP
    (PyTuple.createTuple
       [PyInt.createInt Prims.int_one;
       PyInt.createInt (Prims.of_int (3));
       PyInt.createInt (Prims.of_int (5));
       PyInt.createInt (Prims.of_int (7));
       PyInt.createInt (Prims.of_int (9))])]
let (varnames_0x7fb3abf9d450 : Prims.string Prims.list) = ["d"]
let names_0x7fb3abf9d450 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d450 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7fb3abf9d450 : Prims.string Prims.list) = ["run_for_loop"]
let (co_0x7fb3abf9d450 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d450;
    Structs.co_consts = consts_0x7fb3abf9d450;
    Structs.co_varnames = varnames_0x7fb3abf9d450;
    Structs.co_names = (names_0x7fb3abf9d450 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d450 ());
    Structs.co_freevars = freevars_0x7fb3abf9d450
  }
let (bc_0x7fb3abf9d3a0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_DEREF Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d3a0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyString.createString "0123456789")]
let (varnames_0x7fb3abf9d3a0 : Prims.string Prims.list) = ["s"]
let names_0x7fb3abf9d3a0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d3a0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7fb3abf9d3a0 : Prims.string Prims.list) = ["run_for_loop"]
let (co_0x7fb3abf9d3a0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d3a0;
    Structs.co_consts = consts_0x7fb3abf9d3a0;
    Structs.co_varnames = varnames_0x7fb3abf9d3a0;
    Structs.co_names = (names_0x7fb3abf9d3a0 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d3a0 ());
    Structs.co_freevars = freevars_0x7fb3abf9d3a0
  }
let (bc_0x7fb3abf9d2f0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_DEREF Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d2f0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP
    (PyTuple.createTuple
       [PyInt.createInt Prims.int_zero;
       PyInt.createInt Prims.int_one;
       PyInt.createInt (Prims.of_int (2));
       PyInt.createInt (Prims.of_int (3));
       PyInt.createInt (Prims.of_int (4));
       PyInt.createInt (Prims.of_int (5));
       PyInt.createInt (Prims.of_int (6));
       PyInt.createInt (Prims.of_int (7));
       PyInt.createInt (Prims.of_int (8));
       PyInt.createInt (Prims.of_int (9))])]
let (varnames_0x7fb3abf9d2f0 : Prims.string Prims.list) = ["t"]
let names_0x7fb3abf9d2f0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d2f0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7fb3abf9d2f0 : Prims.string Prims.list) = ["run_for_loop"]
let (co_0x7fb3abf9d2f0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d2f0;
    Structs.co_consts = consts_0x7fb3abf9d2f0;
    Structs.co_varnames = varnames_0x7fb3abf9d2f0;
    Structs.co_names = (names_0x7fb3abf9d2f0 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d2f0 ());
    Structs.co_freevars = freevars_0x7fb3abf9d2f0
  }
let (bc_0x7fb3abf9d240 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (6));
    Structs.LOAD_CONST (Prims.of_int (7));
    Structs.LOAD_CONST (Prims.of_int (8));
    Structs.LOAD_CONST (Prims.of_int (9));
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.BUILD_LIST (Prims.of_int (10));
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_DEREF Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d240 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (4)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (5)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (6)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (8)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (9)))]
let (varnames_0x7fb3abf9d240 : Prims.string Prims.list) = ["l"]
let names_0x7fb3abf9d240 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d240 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (freevars_0x7fb3abf9d240 : Prims.string Prims.list) = ["run_for_loop"]
let (co_0x7fb3abf9d240 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d240;
    Structs.co_consts = consts_0x7fb3abf9d240;
    Structs.co_varnames = varnames_0x7fb3abf9d240;
    Structs.co_names = (names_0x7fb3abf9d240 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d240 ());
    Structs.co_freevars = freevars_0x7fb3abf9d240
  }
let (bc_0x7fb3abf9d190 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.COMPARE_OP Prims.int_zero;
    Structs.POP_JUMP_IF_FALSE (Prims.of_int (22));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.INPLACE_ADD;
    Structs.STORE_FAST Prims.int_zero;
    Structs.JUMP_ABSOLUTE (Prims.of_int (4));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d190 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (10)));
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let (varnames_0x7fb3abf9d190 : Prims.string Prims.list) = ["x"]
let names_0x7fb3abf9d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7fb3abf9d190 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fb3abf9d190 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d190;
    Structs.co_consts = consts_0x7fb3abf9d190;
    Structs.co_varnames = varnames_0x7fb3abf9d190;
    Structs.co_names = (names_0x7fb3abf9d190 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d190 ());
    Structs.co_freevars = (freevars_0x7fb3abf9d190 ())
  }
let (bc_0x7fb3abf9d0e0 : Structs.bytecode) =
  Structs.CODE
    [Structs.BUILD_LIST Prims.int_zero;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.GET_ITER;
    Structs.FOR_ITER (Prims.of_int (14));
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.BUILD_LIST Prims.int_one;
    Structs.INPLACE_ADD;
    Structs.STORE_FAST Prims.int_one;
    Structs.JUMP_ABSOLUTE (Prims.of_int (8));
    Structs.LOAD_FAST Prims.int_one;
    Structs.RETURN_VALUE]
let (consts_0x7fb3abf9d0e0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ())]
let (varnames_0x7fb3abf9d0e0 : Prims.string Prims.list) =
  ["obj"; "obj_elems"; "elem"]
let names_0x7fb3abf9d0e0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7fb3abf9d0e0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7fb3abf9d0e0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fb3abf9d0e0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d0e0;
    Structs.co_consts = consts_0x7fb3abf9d0e0;
    Structs.co_varnames = varnames_0x7fb3abf9d0e0;
    Structs.co_names = (names_0x7fb3abf9d0e0 ());
    Structs.co_cellvars = (cellvars_0x7fb3abf9d0e0 ());
    Structs.co_freevars = (freevars_0x7fb3abf9d0e0 ())
  }
let (consts_0x7fb3abf9d500 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyString.createString "Tests for loops\n    ");
  Structs.CODEOBJECT co_0x7fb3abf9d0e0;
  Structs.PYTYP (PyString.createString "top_level.<locals>.run_for_loop");
  Structs.CODEOBJECT co_0x7fb3abf9d190;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test_while_loop");
  Structs.CODEOBJECT co_0x7fb3abf9d240;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_list_for_loop");
  Structs.CODEOBJECT co_0x7fb3abf9d2f0;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_tuple_for_loop");
  Structs.CODEOBJECT co_0x7fb3abf9d3a0;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_string_for_loop");
  Structs.CODEOBJECT co_0x7fb3abf9d450;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_dict_for_loop")]
let (varnames_0x7fb3abf9d500 : Prims.string Prims.list) =
  ["test_while_loop";
  "test_list_for_loop";
  "test_tuple_for_loop";
  "test_string_for_loop";
  "test_dict_for_loop";
  "result"]
let names_0x7fb3abf9d500 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (cellvars_0x7fb3abf9d500 : Prims.string Prims.list) = ["run_for_loop"]
let freevars_0x7fb3abf9d500 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fb3abf9d500 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fb3abf9d500;
    Structs.co_consts = consts_0x7fb3abf9d500;
    Structs.co_varnames = varnames_0x7fb3abf9d500;
    Structs.co_names = (names_0x7fb3abf9d500 ());
    Structs.co_cellvars = cellvars_0x7fb3abf9d500;
    Structs.co_freevars = (freevars_0x7fb3abf9d500 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7fb3abf9d500
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___