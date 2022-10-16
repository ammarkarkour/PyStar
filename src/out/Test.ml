open Prims
let (bc_0x7f2c2b61cdf0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (6));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.BUILD_LIST (Prims.of_int (3));
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.RETURN_VALUE]
let (bc_0x7f2c2b61cd40 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST (Prims.of_int (4));
    Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST (Prims.of_int (5));
    Structs.BUILD_LIST Prims.int_zero;
    Structs.UNARY_NOT;
    Structs.STORE_FAST (Prims.of_int (6));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.BUILD_LIST Prims.int_one;
    Structs.UNARY_NOT;
    Structs.STORE_FAST (Prims.of_int (7));
    Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST (Prims.of_int (8));
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST (Prims.of_int (9));
    Structs.BUILD_MAP Prims.int_zero;
    Structs.UNARY_NOT;
    Structs.STORE_FAST (Prims.of_int (10));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.BUILD_MAP Prims.int_one;
    Structs.UNARY_NOT;
    Structs.STORE_FAST (Prims.of_int (11));
    Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST (Prims.of_int (12));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.LOAD_FAST (Prims.of_int (4));
    Structs.LOAD_FAST (Prims.of_int (5));
    Structs.LOAD_FAST (Prims.of_int (6));
    Structs.LOAD_FAST (Prims.of_int (7));
    Structs.LOAD_FAST (Prims.of_int (8));
    Structs.LOAD_FAST (Prims.of_int (9));
    Structs.LOAD_FAST (Prims.of_int (10));
    Structs.LOAD_FAST (Prims.of_int (11));
    Structs.LOAD_FAST (Prims.of_int (12));
    Structs.BUILD_TUPLE (Prims.of_int (13));
    Structs.RETURN_VALUE]
let (consts_0x7f2c2b61cd40 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyBool.createBool true);
  Structs.PYTYP (PyBool.createBool false);
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)))]
let (varnames_0x7f2c2b61cd40 : Prims.string Prims.list) =
  ["x11";
  "x12";
  "x21";
  "x22";
  "x31";
  "x32";
  "x41";
  "x42";
  "x51";
  "x52";
  "x61";
  "x62";
  "x7"]
let names_0x7f2c2b61cd40 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f2c2b61cd40 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f2c2b61cd40;
    Structs.co_consts = consts_0x7f2c2b61cd40;
    Structs.co_varnames = varnames_0x7f2c2b61cd40;
    Structs.co_names = (names_0x7f2c2b61cd40 ())
  }
let (bc_0x7f2c2b61cc90 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.INPLACE_SUBTRACT;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7f2c2b61cc90 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let (varnames_0x7f2c2b61cc90 : Prims.string Prims.list) = ["x"]
let names_0x7f2c2b61cc90 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f2c2b61cc90 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f2c2b61cc90;
    Structs.co_consts = consts_0x7f2c2b61cc90;
    Structs.co_varnames = varnames_0x7f2c2b61cc90;
    Structs.co_names = (names_0x7f2c2b61cc90 ())
  }
let (bc_0x7f2c2b61cbe0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.BUILD_LIST Prims.int_one;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.INPLACE_ADD;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.BUILD_LIST Prims.int_one;
    Structs.INPLACE_ADD;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.INPLACE_ADD;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.INPLACE_ADD;
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.BUILD_TUPLE (Prims.of_int (4));
    Structs.RETURN_VALUE]
let (consts_0x7f2c2b61cbe0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyString.createString "");
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyString.createString "1")]
let (varnames_0x7f2c2b61cbe0 : Prims.string Prims.list) =
  ["x1"; "x2"; "x3"; "x4"]
let names_0x7f2c2b61cbe0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f2c2b61cbe0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f2c2b61cbe0;
    Structs.co_consts = consts_0x7f2c2b61cbe0;
    Structs.co_varnames = varnames_0x7f2c2b61cbe0;
    Structs.co_names = (names_0x7f2c2b61cbe0 ())
  }
let (consts_0x7f2c2b61cdf0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.CODEOBJECT co_0x7f2c2b61cbe0;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_UNARY_POSITIVE");
  Structs.CODEOBJECT co_0x7f2c2b61cc90;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_UNARY_NEGATIVE");
  Structs.CODEOBJECT co_0x7f2c2b61cd40;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test_UNARY_NOT")]
let (varnames_0x7f2c2b61cdf0 : Prims.string Prims.list) =
  ["test_UNARY_POSITIVE"; "test_UNARY_NEGATIVE"; "test_UNARY_NOT"; "result"]
let names_0x7f2c2b61cdf0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f2c2b61cdf0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f2c2b61cdf0;
    Structs.co_consts = consts_0x7f2c2b61cdf0;
    Structs.co_varnames = varnames_0x7f2c2b61cdf0;
    Structs.co_names = (names_0x7f2c2b61cdf0 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f2c2b61cdf0
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___