open Prims
let (bc_0x7f5b70673710 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.COMPARE_OP (Prims.of_int (2));
    Structs.POP_JUMP_IF_TRUE (Prims.of_int (12));
    Structs.LOAD_GLOBAL Prims.int_zero;
    Structs.RAISE_VARARGS Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.RETURN_VALUE]
let (consts_0x7f5b70673710 : Structs.pyObj Prims.list) =
  [Structs.PYTYP
     (PyString.createString "Write your own code inside this function\n    ");
  Structs.PYTYP (PyInt.createInt Prims.int_one);
  Structs.PYTYP (PyInt.createInt (Prims.of_int (2)));
  Structs.PYTYP (PyNone.createNone ())]
let varnames_0x7f5b70673710 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (names_0x7f5b70673710 : Prims.string Prims.list) = ["AssertionError"]
let cellvars_0x7f5b70673710 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f5b70673710 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f5b70673710 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f5b70673710;
    Structs.co_consts = consts_0x7f5b70673710;
    Structs.co_varnames = (varnames_0x7f5b70673710 ());
    Structs.co_names = names_0x7f5b70673710;
    Structs.co_cellvars = (cellvars_0x7f5b70673710 ());
    Structs.co_freevars = (freevars_0x7f5b70673710 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f5b70673710
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___