open Prims
let (createSlice :
  Prims.int FStar_Pervasives_Native.option ->
    Prims.int FStar_Pervasives_Native.option ->
      Prims.int FStar_Pervasives_Native.option -> Structs.cls)
  =
  fun start ->
    fun stop ->
      fun step ->
        let obj =
          {
            Structs.name = "slice";
            Structs.pid = Prims.int_zero;
            Structs.value = (Structs.SLICE (start, stop, step));
            Structs.fields = Utils.emptyMap;
            Structs.methods = Utils.emptyMap
          } in
        obj