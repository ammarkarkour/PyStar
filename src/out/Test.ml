open Prims
let (bc_0x7f6b9ce4b9d0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.BINARY_MULTIPLY;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.BINARY_MULTIPLY;
    Structs.STORE_FAST (Prims.of_int (3));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (3));
    Structs.BUILD_TUPLE (Prims.of_int (4));
    Structs.RETURN_VALUE]
let (consts_0x7f6b9ce4b9d0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyInt.createInt (Prims.of_int (3)))]
let (varnames_0x7f6b9ce4b9d0 : Prims.string Prims.list) =
  ["var_1"; "var_2"; "var_3"; "var_4"]
let names_0x7f6b9ce4b9d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f6b9ce4b9d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f6b9ce4b9d0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f6b9ce4b9d0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f6b9ce4b9d0;
    Structs.co_consts = consts_0x7f6b9ce4b9d0;
    Structs.co_varnames = varnames_0x7f6b9ce4b9d0;
    Structs.co_names = (names_0x7f6b9ce4b9d0 ());
    Structs.co_cellvars = (cellvars_0x7f6b9ce4b9d0 ());
    Structs.co_freevars = (freevars_0x7f6b9ce4b9d0 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f6b9ce4b9d0
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___