open Prims
let (createString : Prims.string -> Structs.cls) =
  fun s ->
    let iter =
      FStar_Map.upd Utils.emptyMap "__iter__"
        (Structs.UNFUNOBJ
           (fun a ->
              let next =
                FStar_Map.upd a.Structs.methods "__next__"
                  (Structs.UNFUNBLT
                     (fun b ->
                        match b.Structs.value with
                        | Structs.STRING s1 ->
                            (match FStar_String.list_of_string s1 with
                             | [] -> Structs.EXCEPTION "StopIteration"
                             | x::l ->
                                 let newListIter =
                                   {
                                     Structs.name = "str_iterator";
                                     Structs.pid = Prims.int_zero;
                                     Structs.value =
                                       (Structs.STRING
                                          (FStar_String.string_of_list l));
                                     Structs.fields = (b.Structs.fields);
                                     Structs.methods = (b.Structs.methods)
                                   } in
                                 let nextString =
                                   {
                                     Structs.name = "str_iterator";
                                     Structs.pid = Prims.int_zero;
                                     Structs.value =
                                       (Structs.STRING
                                          (FStar_String.make Prims.int_one x));
                                     Structs.fields = (b.Structs.fields);
                                     Structs.methods = (b.Structs.methods)
                                   } in
                                 Structs.TUPLE [nextString; newListIter])
                        | uu___ -> Structs.EXCEPTION "Str_Iterator Error")) in
              let obj =
                {
                  Structs.name = "str_iterator";
                  Structs.pid = (a.Structs.pid);
                  Structs.value = (a.Structs.value);
                  Structs.fields = (a.Structs.fields);
                  Structs.methods = next
                } in
              obj)) in
    let mul =
      FStar_Map.upd Utils.emptyMap "__mul__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING s1, Structs.BOOL b1) ->
                       if b1 then Structs.STRING s1 else Structs.STRING ""
                   | (Structs.STRING s1, Structs.INT a1) ->
                       if Prims.int_zero >= a1
                       then Structs.STRING ""
                       else
                         Structs.STRING
                           (FStar_String.concat ""
                              (Utils.tabulate (fun x -> s1) a1))
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let add =
      FStar_Map.upd mul "__add__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING s1, Structs.STRING s2) ->
                       Structs.STRING (Prims.strcat s2 s1)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let lt =
      FStar_Map.upd add "__lt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL false
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL false
                        | uu___1 -> Structs.BOOL true)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL true
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL false
                        | uu___1 -> Structs.BOOL true)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL true
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL false
                        | uu___1 -> Structs.BOOL false)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL false
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL true
                        | uu___1 -> Structs.BOOL true)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL false
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL true
                        | uu___1 -> Structs.BOOL false)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING a1, Structs.STRING b1) ->
                       (match FStar_String.compare a1 b1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.BOOL true
                        | uu___1 when uu___1 = Prims.int_one ->
                            Structs.BOOL true
                        | uu___1 -> Structs.BOOL false)
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let subscr =
      FStar_Map.upd ge "__subscr__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.STRING s1, Structs.INT i) ->
                       (match Utils.subString_pos s1 i with
                        | FStar_Pervasives_Native.Some c -> Structs.STRING c
                        | FStar_Pervasives_Native.None ->
                            Structs.EXCEPTION "String Error")
                   | uu___1 -> Structs.EXCEPTION "String Error"))) in
    let allMethods = subscr in
    let obj =
      {
        Structs.name = "str";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.STRING s);
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj