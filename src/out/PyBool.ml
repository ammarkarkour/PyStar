open Prims
let (createBool : Prims.bool -> Structs.cls) =
  fun b ->
    let pos =
      FStar_Map.upd Utils.emptyMap "__pos__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.BOOL b1 ->
                  Structs.INT (if b1 then Prims.int_one else Prims.int_zero)
              | uu___ -> Structs.EXCEPTION "Bool Error")) in
    let neg =
      FStar_Map.upd pos "__neg__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.BOOL b1 ->
                  Structs.INT
                    (if b1 then (Prims.of_int (-1)) else Prims.int_zero)
              | uu___ -> Structs.EXCEPTION "Bool Error")) in
    let bool =
      FStar_Map.upd neg "__bool__"
        (Structs.UNFUNBLT
           (fun a ->
              match a.Structs.value with
              | Structs.BOOL b1 -> Structs.BOOL b1
              | uu___ -> Structs.EXCEPTION "Bool Error")) in
    let mul =
      FStar_Map.upd bool "__mul__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.INT
                         (if b11 && b2 then Prims.int_one else Prims.int_zero)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.INT (if b2 then i else Prims.int_zero)
                   | (Structs.BOOL b2, Structs.LIST l) ->
                       if b2 then Structs.LIST l else Structs.LIST []
                   | (Structs.BOOL b2, Structs.TUPLE t) ->
                       if b2 then Structs.TUPLE t else Structs.TUPLE []
                   | (Structs.BOOL b2, Structs.STRING s) ->
                       if b2 then Structs.STRING s else Structs.STRING ""
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let floordiv =
      FStar_Map.upd mul "__floordiv__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       if Prims.op_Negation b11
                       then Structs.EXCEPTION "Bool Error"
                       else
                         if b2
                         then Structs.INT Prims.int_one
                         else Structs.INT Prims.int_zero
                   | (Structs.BOOL b11, Structs.INT i) ->
                       if Prims.op_Negation b11
                       then Structs.EXCEPTION "Bool Error"
                       else Structs.INT (i / Prims.int_one)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let modulo =
      FStar_Map.upd floordiv "__mod__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       if Prims.op_Negation b11
                       then Structs.EXCEPTION "Bool Error"
                       else Structs.INT Prims.int_zero
                   | (Structs.BOOL b11, Structs.INT i) ->
                       if Prims.op_Negation b11
                       then Structs.EXCEPTION "Bool Error"
                       else Structs.INT Prims.int_zero
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let add =
      FStar_Map.upd modulo "__add__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.INT
                         (if b11 && b2
                          then (Prims.of_int (2))
                          else
                            if b11 || b2
                            then Prims.int_one
                            else Prims.int_zero)
                   | (Structs.BOOL b11, Structs.INT i) ->
                       Structs.INT (if b11 then Prims.int_one + i else i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let sub =
      FStar_Map.upd add "__sub__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.INT
                         (if b11 && b2
                          then Prims.int_zero
                          else
                            if b11
                            then (Prims.of_int (-1))
                            else if b2 then Prims.int_one else Prims.int_zero)
                   | (Structs.BOOL b11, Structs.INT i) ->
                       Structs.INT (if b11 then i - Prims.int_one else i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let lt =
      FStar_Map.upd sub "__lt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.BOOL (if b11 then false else b2)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.BOOL
                         (if b2
                          then Prims.int_one < i
                          else Prims.int_zero < i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.BOOL (if b11 then b2 else true)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.BOOL
                         (if b2
                          then Prims.int_one <= i
                          else Prims.int_zero <= i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.BOOL (b11 = b2)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.BOOL
                         (if b2
                          then i = Prims.int_one
                          else i = Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.INT a1, Structs.INT b2) ->
                       Structs.BOOL (a1 <> b2)
                   | (Structs.INT a1, Structs.BOOL b2) ->
                       Structs.BOOL
                         (if b2
                          then a1 <> Prims.int_one
                          else a1 <> Prims.int_zero)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.BOOL
                         (if b11 then Prims.op_Negation b2 else false)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.BOOL
                         (if b2
                          then Prims.int_one > i
                          else Prims.int_zero > i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b1) ->
                  (match ((a.Structs.value), (b1.Structs.value)) with
                   | (Structs.BOOL b11, Structs.BOOL b2) ->
                       Structs.BOOL
                         (if b11 then true else Prims.op_Negation b2)
                   | (Structs.BOOL b2, Structs.INT i) ->
                       Structs.BOOL
                         (if b2
                          then Prims.int_one >= i
                          else Prims.int_zero >= i)
                   | uu___1 -> Structs.EXCEPTION "Bool Error"))) in
    let allMethods = ge in
    let obj =
      {
        Structs.name = "NoneType";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.BOOL b);
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj