open Prims
let (createNone : unit -> Structs.cls) =
  fun uu___ ->
    let bool =
      FStar_Map.upd Utils.emptyMap "__bool__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.NONE -> Structs.BOOL false
              | uu___1 -> Structs.EXCEPTION "None Error")) in
    let lt =
      FStar_Map.upd bool "__lt__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with | (a, b) -> Structs.EXCEPTION "None Error")) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with | (a, b) -> Structs.EXCEPTION "None Error")) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.NONE, Structs.NONE) -> Structs.BOOL true
                   | uu___2 -> Structs.EXCEPTION "None Error"))) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.NONE, Structs.NONE) -> Structs.BOOL false
                   | uu___2 -> Structs.EXCEPTION "None Error"))) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with | (a, b) -> Structs.EXCEPTION "None Error")) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___1 ->
              match uu___1 with | (a, b) -> Structs.EXCEPTION "None Error")) in
    let allMethods = ge in
    let obj =
      {
        Structs.name = "NoneType";
        Structs.pid = Prims.int_zero;
        Structs.value = Structs.NONE;
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj