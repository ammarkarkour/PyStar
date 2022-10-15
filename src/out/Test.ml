open Prims
let (bc_0x7f92191c0c90 : Structs.bytecode) =
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
    Structs.LOAD_FAST Prims.int_one;
    Structs.CALL_FUNCTION Prims.int_zero;
    Structs.BINARY_MULTIPLY;
    Structs.RETURN_VALUE]
let (bc_0x7f92191c0be0 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f92191c0be0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)))]
let varnames_0x7f92191c0be0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f92191c0be0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f92191c0be0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f92191c0be0;
    Structs.co_consts = consts_0x7f92191c0be0;
    Structs.co_varnames = (varnames_0x7f92191c0be0 ());
    Structs.co_names = (names_0x7f92191c0be0 ())
  }
let (bc_0x7f92191c0a80 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (consts_0x7f92191c0a80 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt (Prims.of_int (7)))]
let varnames_0x7f92191c0a80 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let names_0x7f92191c0a80 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f92191c0a80 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f92191c0a80;
    Structs.co_consts = consts_0x7f92191c0a80;
    Structs.co_varnames = (varnames_0x7f92191c0a80 ());
    Structs.co_names = (names_0x7f92191c0a80 ())
  }
let (consts_0x7f92191c0c90 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.CODEOBJECT co_0x7f92191c0a80;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test");
  Structs.CODEOBJECT co_0x7f92191c0be0;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test2")]
let (varnames_0x7f92191c0c90 : Prims.string Prims.list) = ["test"; "test2"]
let names_0x7f92191c0c90 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f92191c0c90 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f92191c0c90;
    Structs.co_consts = consts_0x7f92191c0c90;
    Structs.co_varnames = varnames_0x7f92191c0c90;
    Structs.co_names = (names_0x7f92191c0c90 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f92191c0c90
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___