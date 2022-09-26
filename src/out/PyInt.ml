open Prims
let (createInt : Prims.int -> Structs.cls) =
  fun v ->
    let pos =
      FStar_Map.upd Utils.emptyMap "__pos__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.INT x -> Structs.INT x
              | uu___ -> Structs.EXCEPTION "Int Error")) in
    let neg =
      FStar_Map.upd pos "__neg__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.INT a1 -> Structs.INT (- a1)
              | uu___ -> Structs.EXCEPTION "Int Error")) in
    let bool =
      FStar_Map.upd neg "__bool__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.INT a1 ->
                  Structs.BOOL (if a1 = Prims.int_zero then false else true)
              | uu___ -> Structs.EXCEPTION "Int Error")) in
    let mul =
      FStar_Map.upd bool "__mul__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.INT (b1 * a1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.INT (if b1 then a1 else Prims.int_zero)
                   | (Structs.INT a1, Structs.STRING s) ->
                       if Prims.int_zero >= a1
                       then Structs.STRING ""
                       else
                         Structs.STRING
                           (FStar_String.concat ""
                              (Utils.tabulate (fun x -> s) a1))
                   | (Structs.INT a1, Structs.LIST l) ->
                       if Prims.int_zero >= a1
                       then Structs.LIST []
                       else
                         Structs.LIST
                           (FStar_List_Tot_Base.fold_left
                              (fun x ->
                                 fun y -> FStar_List_Tot_Base.append x y) []
                              (Utils.tabulate (fun x -> l) a1))
                   | (Structs.INT a1, Structs.TUPLE l) ->
                       if Prims.int_zero >= a1
                       then Structs.TUPLE []
                       else
                         Structs.TUPLE
                           (FStar_List_Tot_Base.fold_left
                              (fun x ->
                                 fun y -> FStar_List_Tot_Base.append x y) []
                              (Utils.tabulate (fun x -> l) a1))
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let floordiv =
      FStar_Map.upd mul "__floordiv__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       if a1 = Prims.int_zero
                       then Structs.EXCEPTION "Int Error"
                       else Structs.INT (b1 / a1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       if a1 = Prims.int_zero
                       then Structs.EXCEPTION "Int Error"
                       else
                         if b1
                         then Structs.INT (Prims.int_one / a1)
                         else Structs.INT Prims.int_zero
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let modulo =
      FStar_Map.upd floordiv "__mod__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       if a1 = Prims.int_zero
                       then Structs.EXCEPTION "Int Error"
                       else Structs.INT (b1 mod a1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       if a1 = Prims.int_zero
                       then Structs.EXCEPTION "Int Error"
                       else
                         if b1
                         then Structs.INT (Prims.int_one mod a1)
                         else Structs.INT Prims.int_zero
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let add =
      FStar_Map.upd modulo "__add__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.INT (a1 + b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.INT (if b1 then Prims.int_one + a1 else a1)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let sub =
      FStar_Map.upd add "__sub__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.INT (b1 - a1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.INT (if b1 then Prims.int_one + a1 else a1)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let lt =
      FStar_Map.upd sub "__lt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 < b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 < Prims.int_one
                          else a1 < Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 <= b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 <= Prims.int_one
                          else a1 <= Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 = b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 = Prims.int_one
                          else a1 = Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 <> b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 <> Prims.int_one
                          else a1 <> Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 > b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 > Prims.int_one
                          else a1 > Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b1) ->
                       Structs.BOOL (a1 >= b1)
                   | (Structs.INT a1, Structs.BOOL b1) ->
                       Structs.BOOL
                         (if b1
                          then a1 >= Prims.int_one
                          else a1 >= Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Int Error"))) in
    let allMethods = ge in
    let obj =
      {
        Structs.name = "int";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.INT v);
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj