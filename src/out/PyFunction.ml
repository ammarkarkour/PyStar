open Prims
let (createFunction : Structs.functionObj -> Structs.cls) =
  fun f ->
    let lt =
      FStar_Map.upd Utils.emptyMap "__lt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with | (a, b) -> Structs.EXCEPTION "Funtion Error")) in
    let allMethods = ge in
    let obj =
      {
        Structs.name = "function";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.FUNCTION f);
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj