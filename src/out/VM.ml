open Prims
let (runCode : Structs.codeObj -> Structs.pyObj) =
  fun code ->
    let initVM =
      {
        Structs.callStack = [];
        Structs.code = code;
        Structs.vmpid = Prims.int_one;
        Structs.idCount = Prims.int_one;
        Structs.usedIds = Utils.idsMap
      } in
    let uu___ =
      Exec.makeFrame initVM code [] Utils.emptyMap Utils.emptyMap
        Utils.emptyMap in
    match uu___ with
    | (virM, globalFrame) ->
        let uu___1 = Exec.runFrame virM globalFrame in
        (match uu___1 with | (finalVM, result) -> result)
let (runCode_returnVM : Structs.codeObj -> (Structs.vm * Structs.pyObj)) =
  fun code ->
    let initVM =
      {
        Structs.callStack = [];
        Structs.code = code;
        Structs.vmpid = Prims.int_one;
        Structs.idCount = Prims.int_one;
        Structs.usedIds = Utils.idsMap
      } in
    let uu___ =
      Exec.makeFrame initVM code [] Utils.emptyMap Utils.emptyMap
        Utils.emptyMap in
    match uu___ with
    | (virM, globalFrame) ->
        let uu___1 = Exec.runFrame virM globalFrame in
        (match uu___1 with | (finalVM, result) -> (finalVM, result))