open Prims
let (createException : Prims.string -> Structs.cls) =
  fun s ->
    let obj =
      {
        Structs.name = "Exception";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.EXCEPTION s);
        Structs.fields = Utils.emptyMap;
        Structs.methods = Utils.emptyMap
      } in
    obj