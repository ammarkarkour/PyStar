open Prims
let (bc_0x7f6a154adea0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.MAKE_FUNCTION Prims.int_zero;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (3));
    Structs.CALL_FUNCTION Prims.int_one;
    Structs.RETURN_VALUE]
let (bc_0x7f6a154addf0 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST Prims.int_one;
    Structs.COMPARE_OP (Prims.of_int (4));
    Structs.POP_JUMP_IF_FALSE (Prims.of_int (18));
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.INPLACE_SUBTRACT;
    Structs.STORE_FAST Prims.int_zero;
    Structs.JUMP_ABSOLUTE Prims.int_zero;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.RETURN_VALUE]
let (consts_0x7f6a154addf0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP (PyNone.createNone ());
  Structs.PYTYP (PyInt.createInt Prims.int_zero);
  Structs.PYTYP (PyInt.createInt Prims.int_one)]
let (varnames_0x7f6a154addf0 : Prims.string Prims.list) = ["n"]
let names_0x7f6a154addf0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f6a154addf0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f6a154addf0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f6a154addf0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f6a154addf0;
    Structs.co_consts = consts_0x7f6a154addf0;
    Structs.co_varnames = varnames_0x7f6a154addf0;
    Structs.co_names = (names_0x7f6a154addf0 ());
    Structs.co_cellvars = (cellvars_0x7f6a154addf0 ());
    Structs.co_freevars = (freevars_0x7f6a154addf0 ())
  }
let (consts_0x7f6a154adea0 : Structs.pyObj Prims.list) =
  [Structs.PYTYP
     (PyString.createString "Testing running time for while loop \n    ");
  Structs.CODEOBJECT co_0x7f6a154addf0;
  Structs.PYTYP (PyString.createString "top_level.<locals>.test_while_loop");
  Structs.PYTYP (PyInt.createInt (Prims.of_int (1000)))]
let (varnames_0x7f6a154adea0 : Prims.string Prims.list) = ["test_while_loop"]
let names_0x7f6a154adea0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let cellvars_0x7f6a154adea0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let freevars_0x7f6a154adea0 : 'uuuuu . unit -> 'uuuuu Prims.list =
  fun uu___ -> []
let (co_0x7f6a154adea0 : Structs.codeObj) =
  {
    Structs.co_code = bc_0x7f6a154adea0;
    Structs.co_consts = consts_0x7f6a154adea0;
    Structs.co_varnames = varnames_0x7f6a154adea0;
    Structs.co_names = (names_0x7f6a154adea0 ());
    Structs.co_cellvars = (cellvars_0x7f6a154adea0 ());
    Structs.co_freevars = (freevars_0x7f6a154adea0 ())
  }
let (uu___0 : (Structs.vm * Structs.pyObj)) =
  VM.runCode_returnVM co_0x7f6a154adea0
let (virt_m : Structs.vm) = match uu___0 with | (virt_m1, res) -> virt_m1
let (res : Structs.pyObj) = match uu___0 with | (virt_m1, res1) -> res1
let (print_program_state : unit) =
  let uu___ = Utils.print_program_state virt_m res in
  FStar_IO.print_string uu___