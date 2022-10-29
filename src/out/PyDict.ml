open Prims
let rec (is_hashable_tuple : Structs.cls Prims.list -> Prims.bool) =
  fun l ->
    match l with
    | [] -> true
    | x::l2 -> (is_hashable_key x) && (is_hashable_tuple l2)
and (is_hashable_key : Structs.cls -> Prims.bool) =
  fun k ->
    match k.Structs.value with
    | Structs.BOOL b -> true
    | Structs.STRING s -> true
    | Structs.INT i -> true
    | Structs.TUPLE l -> is_hashable_tuple l
    | Structs.USERDEF ->
        (match FStar_Map.sel k.Structs.methods "__hash__" with
         | Structs.ERR err -> false
         | uu___ ->
             (match FStar_Map.sel k.Structs.methods "__eq__" with
              | Structs.ERR err -> false
              | uu___1 -> true))
    | uu___ -> false
let rec (is_hashable : Structs.cls Prims.list -> Prims.bool) =
  fun kl ->
    match kl with
    | [] -> true
    | k::kl' -> (is_hashable_key k) && (is_hashable kl')
let (createDict : (Structs.cls * Structs.cls) Prims.list -> Structs.cls) =
  fun vkl ->
    let uu___ = FStar_List_Tot_Base.unzip vkl in
    match uu___ with
    | (vl, kl) ->
        if is_hashable kl
        then
          let contains =
            FStar_Map.upd Utils.emptyMap "__contains__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) ->
                        (match FStar_Map.sel a.Structs.fields "keys" with
                         | Structs.PYTYP objakl ->
                             (match objakl.Structs.value with
                              | Structs.LIST kl1 ->
                                  Structs.BOOL (Utils.list_contains kl1 b)
                              | uu___2 ->
                                  Structs.EXCEPTION "Dictionary Error")
                         | uu___2 -> Structs.EXCEPTION "Dictionary Error"))) in
          let lt =
            FStar_Map.upd contains "__lt__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) -> Structs.EXCEPTION "Dictionary Error")) in
          let le =
            FStar_Map.upd lt "__le__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) -> Structs.EXCEPTION "Dictionary Error")) in
          let eq =
            FStar_Map.upd le "__eq__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) ->
                        (match ((a.Structs.value), (b.Structs.value)) with
                         | (Structs.DICT vkl1, Structs.DICT vkl2) ->
                             let uu___2 = FStar_List_Tot_Base.unzip vkl1 in
                             (match uu___2 with
                              | (vkl1_vals, vkl1_keys) ->
                                  let uu___3 = FStar_List_Tot_Base.unzip vkl2 in
                                  (match uu___3 with
                                   | (vkl2_vals, vkl2_keys) ->
                                       (match ((Utils.list_lex_eq vkl1_keys
                                                  vkl2_keys),
                                                (Utils.list_lex_eq vkl1_vals
                                                   vkl2_vals))
                                        with
                                        | (Structs.BOOL b1, Structs.BOOL b2)
                                            -> Structs.BOOL (b1 && b2)
                                        | uu___4 -> Structs.BOOL false)))
                         | uu___2 -> Structs.EXCEPTION "Dictionary Error"))) in
          let neq =
            FStar_Map.upd eq "__ne__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) ->
                        (match ((a.Structs.value), (b.Structs.value)) with
                         | (Structs.DICT vkl1, Structs.DICT vkl2) ->
                             let uu___2 = FStar_List_Tot_Base.unzip vkl1 in
                             (match uu___2 with
                              | (vkl1_vals, vkl1_keys) ->
                                  let uu___3 = FStar_List_Tot_Base.unzip vkl2 in
                                  (match uu___3 with
                                   | (vkl2_vals, vkl2_keys) ->
                                       (match ((Utils.list_lex_ne vkl1_keys
                                                  vkl2_keys),
                                                (Utils.list_lex_ne vkl1_vals
                                                   vkl2_vals))
                                        with
                                        | (Structs.BOOL b1, Structs.BOOL b2)
                                            -> Structs.BOOL (b1 && b2)
                                        | uu___4 -> Structs.BOOL false)))
                         | uu___2 -> Structs.EXCEPTION "Dictionary Error"))) in
          let gt =
            FStar_Map.upd neq "__gt__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) -> Structs.EXCEPTION "Dictionary Error")) in
          let ge =
            FStar_Map.upd gt "__ge__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) -> Structs.EXCEPTION "Dictionary Error")) in
          let subscr =
            FStar_Map.upd ge "__subscr__"
              (Structs.BINFUNBLT
                 (fun uu___1 ->
                    match uu___1 with
                    | (a, b) ->
                        (match ((a.Structs.value), (b.Structs.value)) with
                         | (Structs.DICT vkl1, key) ->
                             (match FStar_List_Tot_Base.find
                                      (fun x ->
                                         match x with
                                         | (v, k) -> Utils.objEq b k) vkl1
                              with
                              | FStar_Pervasives_Native.None ->
                                  Structs.EXCEPTION "Dictionary Error"
                              | FStar_Pervasives_Native.Some (v, k) ->
                                  v.Structs.value)
                         | uu___2 -> Structs.EXCEPTION "Dictionary Error"))) in
          let keys =
            FStar_Map.upd Utils.emptyMap "keys"
              (Structs.PYTYP (PyList.createList kl)) in
          let values =
            FStar_Map.upd keys "values"
              (Structs.PYTYP (PyList.createList vl)) in
          let obj =
            {
              Structs.name = "dict";
              Structs.pid = Prims.int_zero;
              Structs.value = (Structs.DICT vkl);
              Structs.fields = values;
              Structs.methods = subscr
            } in
          obj
        else PyException.createException "Key is not hashable"