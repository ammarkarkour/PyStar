open Prims
let (bc_0x7f064a1f95b0 : Structs.bytecode) =
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
let (bc_0x7f064a1f9500 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7f064a1f9500 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)))]
let (varnames_0x7f064a1f9500 : Prims.string Prims.list) = ["y"]
let names_0x7f064a1f9500 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f064a1f9500 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f064a1f9500;
    Structs.co_consts = consts_0x7f064a1f9500;
    Structs.co_varnames = varnames_0x7f064a1f9500;
    Structs.co_names = (names_0x7f064a1f9500 ())
  }
let (bc_0x7f064a1f93a0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7f064a1f93a0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (5)))]
let (varnames_0x7f064a1f93a0 : Prims.string Prims.list) = ["x"]
let names_0x7f064a1f93a0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f064a1f93a0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f064a1f93a0;
    Structs.co_consts = consts_0x7f064a1f93a0;
    Structs.co_varnames = varnames_0x7f064a1f93a0;
    Structs.co_names = (names_0x7f064a1f93a0 ())
  }
let (consts_0x7f064a1f95b0 : Structs.pyObj Prims.list) =
  [Structs.CODEOBJECT co_0x7f064a1f93a0;
  Structs.PYTYP (PyString.createString "first_function");
  Structs.CODEOBJECT co_0x7f064a1f9500;
  Structs.PYTYP (PyString.createString "second_function");
  Structs.PYTYP (PyNone.createNone ())]
let varnames_0x7f064a1f95b0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (names_0x7f064a1f95b0 : Prims.string Prims.list) =
  ["first_function"; "second_function"]
let (co_0x7f064a1f95b0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f064a1f95b0;
    Structs.co_consts = consts_0x7f064a1f95b0;
    Structs.co_varnames = (varnames_0x7f064a1f95b0 ());
    Structs.co_names = names_0x7f064a1f95b0
  }
let t10 = Sys.time()
let (res : Structs.pyObj) = VM.runCode co_0x7f064a1f95b0
let t2 = Sys.time()  
let _ = Printf.printf "%f\n" t10
let _ = Printf.printf "%f\n" t2