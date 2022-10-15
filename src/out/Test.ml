open Prims
let (bc_0x7f14c4a8fbe0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.LOAD_CONST (Prims.of_int (4));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_one;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.BINARY_MULTIPLY;
    Structs.RETURN_VALUE]
let (bc_0x7f14c4a8fb30 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f14c4a8fb30 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)))]
let varnames_0x7f14c4a8fb30 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f14c4a8fb30 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f14c4a8fb30 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f14c4a8fb30;
    Structs.co_consts = consts_0x7f14c4a8fb30;
    Structs.co_varnames = (varnames_0x7f14c4a8fb30 ());
    Structs.co_names = (names_0x7f14c4a8fb30 ())
  }
let (bc_0x7f14c4a8f9d0 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f14c4a8f9d0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)))]
let varnames_0x7f14c4a8f9d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f14c4a8f9d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f14c4a8f9d0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f14c4a8f9d0;
    Structs.co_consts = consts_0x7f14c4a8f9d0;
    Structs.co_varnames = (varnames_0x7f14c4a8f9d0 ());
    Structs.co_names = (names_0x7f14c4a8f9d0 ())
  }
let (consts_0x7f14c4a8fbe0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.CODEOBJECT co_0x7f14c4a8f9d0;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test");
  Structs.CODEOBJECT co_0x7f14c4a8fb30;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test2")]
let (varnames_0x7f14c4a8fbe0 : Prims.string Prims.list) =
  ["test"; "test2"; "x"; "y"]
let names_0x7f14c4a8fbe0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f14c4a8fbe0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f14c4a8fbe0;
    Structs.co_consts = consts_0x7f14c4a8fbe0;
    Structs.co_varnames = varnames_0x7f14c4a8fbe0;
    Structs.co_names = (names_0x7f14c4a8fbe0 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f14c4a8fbe0
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___