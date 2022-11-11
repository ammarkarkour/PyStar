open Prims
let (createList : Structs.cls Prims.list -> Structs.cls) =
  fun l ->
    let iter =
      FStar_Map.upd Utils.emptyMap "__iter__"
        (Structs.UNFUNOBJ
           (fun a ->
              let next =
                FStar_Map.upd a.Structs.methods "__next__"
                  (Structs.UNFUNBLT
                     (fun b ->
                        match b.Structs.value with
                        | Structs.LIST [] ->
                            Structs.EXCEPTION "StopIteration"
                        | Structs.LIST (x::l1) ->
                            let newListIter =
                              {
                                Structs.name = "list_iterator";
                                Structs.pid = Prims.int_zero;
                                Structs.value = (Structs.LIST l1);
                                Structs.fields = (b.Structs.fields);
                                Structs.methods = (b.Structs.methods)
                              } in
                            Structs.TUPLE [x; newListIter]
                        | uu___ -> Structs.EXCEPTION "List_Iterator Error")) in
              let obj =
                {
                  Structs.name = "list_iterator";
                  Structs.pid = (a.Structs.pid);
                  Structs.value = (a.Structs.value);
                  Structs.fields = (a.Structs.fields);
                  Structs.methods = next
                } in
              obj)) in
    let mul =
      FStar_Map.upd iter "__mul__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.BOOL b1) ->
                       if b1 then Structs.LIST l1 else Structs.LIST []
                   | (Structs.LIST l1, Structs.INT a1) ->
                       if Prims.int_zero >= a1
                       then Structs.LIST []
                       else
                         Structs.LIST
                           (FStar_List_Tot_Base.fold_left
                              (fun x ->
                                 fun y -> FStar_List_Tot_Base.append x y) []
                              (Utils.tabulate (fun x -> l1) a1))
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let add =
      FStar_Map.upd mul "__add__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       Structs.LIST (FStar_List_Tot_Base.append l1 l2)
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let contains =
      FStar_Map.upd add "__contains__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), b) with
                   | (Structs.LIST l1, obj) ->
                       Structs.BOOL (Utils.list_contains l1 obj)
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let lt =
      FStar_Map.upd contains "__lt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_lt l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let le =
      FStar_Map.upd lt "__le__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_le l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let eq =
      FStar_Map.upd le "__eq__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_eq l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let neq =
      FStar_Map.upd eq "__ne__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_ne l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let gt =
      FStar_Map.upd neq "__gt__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_gt l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let ge =
      FStar_Map.upd gt "__ge__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.LIST l2) ->
                       (match Utils.list_lex_ge l1 l2 with
                        | Structs.BOOL b1 -> Structs.BOOL b1
                        | uu___1 -> Structs.EXCEPTION "List Error")
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let subscr =
      FStar_Map.upd ge "__subscr__"
        (Structs.BINFUNBLT
           (fun uu___ ->
              match uu___ with
              | (a, b) ->
                  (match ((a.Structs.value), (b.Structs.value)) with
                   | (Structs.LIST l1, Structs.INT i) ->
                       let l_len = FStar_List_Tot_Base.length l1 in
                       if i >= Prims.int_zero
                       then
                         (if i < l_len
                          then
                            match Utils.nth_int l1 i with
                            | FStar_Pervasives_Native.None ->
                                Structs.EXCEPTION "List error"
                            | FStar_Pervasives_Native.Some c ->
                                c.Structs.value
                          else Structs.EXCEPTION "List Error")
                       else
                         (let new_i = l_len + i in
                          if new_i < l_len
                          then
                            match Utils.nth_int l1 new_i with
                            | FStar_Pervasives_Native.None ->
                                Structs.EXCEPTION "List error"
                            | FStar_Pervasives_Native.Some c ->
                                c.Structs.value
                          else Structs.EXCEPTION "List error")
                   | (Structs.LIST l1, Structs.SLICE (start, stop, step)) ->
                       let step1 =
                         match step with
                         | FStar_Pervasives_Native.None -> Prims.int_one
                         | FStar_Pervasives_Native.Some s -> s in
                       (match step1 with
                        | uu___1 when uu___1 = Prims.int_zero ->
                            Structs.EXCEPTION "List Error"
                        | uu___1 ->
                            if
                              (start <> FStar_Pervasives_Native.None) &&
                                (start = stop)
                            then Structs.LIST []
                            else
                              (let l_len = FStar_List_Tot_Base.length l1 in
                               let start1 =
                                 match start with
                                 | FStar_Pervasives_Native.None ->
                                     if step1 < Prims.int_zero
                                     then l_len - Prims.int_one
                                     else Prims.int_zero
                                 | FStar_Pervasives_Native.Some s -> s in
                               let stop1 =
                                 match stop with
                                 | FStar_Pervasives_Native.None ->
                                     if step1 < Prims.int_zero
                                     then (Prims.of_int (-1))
                                     else l_len
                                 | FStar_Pervasives_Native.Some s -> s in
                               Structs.LIST
                                 (Utils.get_slice l1 start1 stop1 step1)))
                   | uu___1 -> Structs.EXCEPTION "List Error"))) in
    let allMethods = subscr in
    let obj =
      {
        Structs.name = "list";
        Structs.pid = Prims.int_zero;
        Structs.value = (Structs.LIST l);
        Structs.fields = Utils.emptyMap;
        Structs.methods = allMethods
      } in
    obj