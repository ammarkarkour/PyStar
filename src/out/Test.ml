open Prims
let (bc_0x7fefbe736660 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_one;
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_NAME Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_NAME Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.RETURN_VALUE]
let (bc_0x7fefbe7365b0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7fefbe7365b0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)))]
let (varnames_0x7fefbe7365b0 : Prims.string Prims.list) = ["y"]
let names_0x7fefbe7365b0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fefbe7365b0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fefbe7365b0;
    Structs.co_consts = consts_0x7fefbe7365b0;
    Structs.co_varnames = varnames_0x7fefbe7365b0;
    Structs.co_names = (names_0x7fefbe7365b0 ())
  }
let (bc_0x7fefbe736450 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7fefbe736450 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (5)))]
let (varnames_0x7fefbe736450 : Prims.string Prims.list) = ["x"]
let names_0x7fefbe736450 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7fefbe736450 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fefbe736450;
    Structs.co_consts = consts_0x7fefbe736450;
    Structs.co_varnames = varnames_0x7fefbe736450;
    Structs.co_names = (names_0x7fefbe736450 ())
  }
let (consts_0x7fefbe736660 : Structs.pyObj Prims.list) =
  [Structs.CODEOBJECT co_0x7fefbe736450;
  Structs.PYTYP (PyString.createString "first_function");
  Structs.CODEOBJECT co_0x7fefbe7365b0;
  Structs.PYTYP (PyString.createString "second_function");
  Structs.PYTYP (PyNone.createNone ())]
let varnames_0x7fefbe736660 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (names_0x7fefbe736660 : Prims.string Prims.list) =
  ["first_function"; "second_function"]
let (co_0x7fefbe736660 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7fefbe736660;
    Structs.co_consts = consts_0x7fefbe736660;
    Structs.co_varnames = (varnames_0x7fefbe736660 ());
    Structs.co_names = names_0x7fefbe736660
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7fefbe736660
let (res : Structs.vm) = match uu___0 with | (res1, virt_m) -> res1
let (virt_m : Structs.pyObj) = match uu___0 with | (res1, virt_m1) -> virt_m1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state res virt_m in
  FStar_IO.print_string uu___