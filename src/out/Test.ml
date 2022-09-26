open Prims
let (bc1 : Structs.bytecode) =
  Structs.CODE [Structs.LOAD_CONST Prims.int_one; Structs.RETURN_VALUE]
let (co_test1 : Structs.codeObj) =
  {
    Structs.co_code = bc1;
    Structs.co_consts =
      [Structs.PYTYP (PyNone.createNone ());
      Structs.PYTYP (PyInt.createInt (Prims.of_int (3)))];
    Structs.co_varnames = [];
    Structs.co_names = []
  }
let (bc2 : Structs.bytecode) =
  Structs.CODE
    [Structs.LOAD_CONST Prims.int_one;
    Structs.STORE_FAST Prims.int_zero;
    Structs.LOAD_CONST (Prims.of_int (2));
    Structs.STORE_FAST Prims.int_one;
    Structs.LOAD_FAST Prims.int_zero;
    Structs.LOAD_FAST Prims.int_one;
    Structs.BINARY_ADD;
    Structs.STORE_FAST (Prims.of_int (2));
    Structs.LOAD_FAST (Prims.of_int (2));
    Structs.RETURN_VALUE]
let (co_test2 : Structs.codeObj) =
  {
    Structs.co_code = bc2;
    Structs.co_consts =
      [Structs.PYTYP (PyNone.createNone ());
      Structs.PYTYP (PyInt.createInt Prims.int_one);
      Structs.PYTYP (PyInt.createInt (Prims.of_int (2)))];
    Structs.co_varnames = ["x"; "y"; "Z"];
    Structs.co_names = []
  }
let (res : Structs.pyObj) = VM.runCode co_test2