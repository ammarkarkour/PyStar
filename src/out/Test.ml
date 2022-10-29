open Prims
let (bc_0x7fec7faa6a80 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.BUILD_LIST Prims.int_one;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST Prims.int_one;
    Structs.RETURN_VALUE]
let (bc_0x7fec7faa69d0 : Structs.bytecode) =
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
    Structs.BUILD_LIST (Prims.of_int (9));
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.BUILD_SLICE (Prims.of_int (2));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.BUILD_SLICE (Prims.of_int (3));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.BUILD_SLICE (Prims.of_int (3));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (11));
    Structs.BUILD_SLICE (Prims.of_int (3));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (4));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (12));
    Structs.LOAD_CONST (Prims.of_int (13));
    Structs.BUILD_SLICE (Prims.of_int (2));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (5));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (14));
    Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.BUILD_SLICE (Prims.of_int (3));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (6));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST Prims.int_one;
    Structs.BUILD_SLICE (Prims.of_int (2));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (7));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (5));
    Structs.LOAD_CONST (Prims.of_int (10));
    Structs.BUILD_SLICE (Prims.of_int (3));
    Structs.BINARY_SUBSCR;
    Structs.STORE_FAST (Prims.of_int (8));
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.LOAD_FAST (Prims.of_int (4));
    Structs.LOAD_FAST (Prims.of_int (5));
    Structs.LOAD_FAST (Prims.of_int (6));
    Structs.LOAD_FAST (Prims.of_int (7));
    Structs.LOAD_FAST (Prims.of_int (8));
    Structs.BUILD_TUPLE (Prims.of_int (8));
    Structs.RETURN_VALUE]
let (consts_0x7fec7faa69d0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (4)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (5)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (6)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (8)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (9)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (-1)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (-2)));
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (15)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (-10)))]
let (varnames_0x7fec7faa69d0 : Prims.string Prims.list) =
  ["l2"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7"; "x8"]
let names_0x7fec7faa69d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fec7faa69d0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fec7faa69d0;
    Structs.co_consts = consts_0x7fec7faa69d0;
    Structs.co_varnames = varnames_0x7fec7faa69d0;
    Structs.co_names = (names_0x7fec7faa69d0 ())
  }
let (consts_0x7fec7faa6a80 : Structs.pyObj Prims.list) =
  [Structs.PYTYP
     (PyString.createString "Test cases for Binary operations\n    ");
  Structs.CODEOBJECT co_0x7fec7faa69d0;
  Structs.PYTYP
    (PyString.createString "top_level.<locals>.test_BINARY_SUBSCR_SLICES")]
let (varnames_0x7fec7faa6a80 : Prims.string Prims.list) =
  ["test_BINARY_SUBSCR_SLICES"; "result"]
let names_0x7fec7faa6a80 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fec7faa6a80 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fec7faa6a80;
    Structs.co_consts = consts_0x7fec7faa6a80;
    Structs.co_varnames = varnames_0x7fec7faa6a80;
    Structs.co_names = (names_0x7fec7faa6a80 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7fec7faa6a80
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___