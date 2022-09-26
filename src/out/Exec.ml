open Prims
let (builtinsToPyObj : Structs.builtins -> Structs.cls) =
  fun bltin ->
    match bltin with
    | Structs.INT n -> PyInt.createInt n
    | Structs.STRING s -> PyString.createString s
    | Structs.BOOL b -> PyBool.createBool b
    | Structs.LIST l -> PyList.createList l
    | Structs.TUPLE t -> PyTuple.createTuple t
    | Structs.DICT kvl -> PyDict.createDict kvl
    | Structs.FUNCTION f -> PyFunction.createFunction f
    | Structs.EXCEPTION s -> PyException.createException s
    | Structs.NONE -> PyNone.createNone ()
    | Structs.USERDEF ->
        PyException.createException "Creating_userdefined Error"
let (makeFrame :
  Structs.vm ->
    Structs.codeObj ->
      Structs.pyObj Prims.list ->
        (Prims.string, Structs.pyObj) FStar_Map.t ->
          (Prims.string, Structs.pyObj) FStar_Map.t ->
            (Structs.vm * Structs.frameObj))
  =
  fun virM ->
    fun code ->
      fun localplus ->
        fun global_names ->
          fun local_names ->
            let frame =
              {
                Structs.dataStack = [];
                Structs.blockStack = [];
                Structs.fCode = code;
                Structs.pc = Prims.int_zero;
                Structs.f_localplus = localplus;
                Structs.f_globals = global_names;
                Structs.f_locals = local_names;
                Structs.f_idCount = (virM.Structs.idCount);
                Structs.f_usedIds = (virM.Structs.usedIds)
              } in
            let newVM =
              {
                Structs.callStack = (frame :: (virM.Structs.callStack));
                Structs.code = (virM.Structs.code);
                Structs.vmpid = (virM.Structs.vmpid);
                Structs.idCount = (virM.Structs.idCount);
                Structs.usedIds = (virM.Structs.usedIds)
              } in
            (newVM, frame)
let (call_function :
  Prims.nat ->
    (Prims.string, Structs.pyObj) FStar_Map.t ->
      Structs.pyObj Prims.list ->
        Prims.nat ->
          (Structs.hashable, Prims.nat) FStar_Map.t ->
            Structs.pyObj Prims.list)
  =
  fun i ->
    fun globals ->
      fun dataStack ->
        fun id ->
          fun usedIds ->
            let uu___ = FStar_List_Tot_Base.splitAt i dataStack in
            match uu___ with
            | (args, newDataStack) ->
                let localplus = FStar_List_Tot_Base.rev args in
                let uu___1 =
                  FStar_List_Tot_Base.splitAt Prims.int_one newDataStack in
                (match uu___1 with
                 | (code, restStack) ->
                     (match FStar_List_Tot_Base.nth code Prims.int_zero with
                      | FStar_Pervasives_Native.None ->
                          (Utils.undefinedBehavior "call_function_1") ::
                          dataStack
                      | FStar_Pervasives_Native.Some (Structs.CODEOBJECT co)
                          ->
                          let newFrame =
                            {
                              Structs.dataStack = [];
                              Structs.blockStack = [];
                              Structs.fCode = co;
                              Structs.pc = Prims.int_zero;
                              Structs.f_localplus = localplus;
                              Structs.f_globals = globals;
                              Structs.f_locals = Utils.emptyMap;
                              Structs.f_idCount = id;
                              Structs.f_usedIds = usedIds
                            } in
                          (Structs.FRAMEOBJECT newFrame) :: restStack
                      | uu___2 -> (Utils.undefinedBehavior "call_function_2")
                          :: dataStack))
let (pop_top : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack -> FStar_List_Tot_Base.tail datastack

let (rot_two : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let tos1 = FStar_List_Tot_Base.nth datastack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None -> (Utils.undefinedBehavior "rot_two") ::
        datastack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) datastack in
        (match uu___ with
         | (uu___1, rest_stack) -> tos11 :: tos :: rest_stack)

let (rot_three : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let tos1 = FStar_List_Tot_Base.nth datastack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None -> (Utils.undefinedBehavior "rot_three_1")
        :: datastack
    | FStar_Pervasives_Native.Some tos11 ->
        let tos2 = FStar_List_Tot_Base.nth datastack (Prims.of_int (2)) in
        (match tos2 with
         | FStar_Pervasives_Native.None ->
             (Utils.undefinedBehavior "rot_three_2") :: datastack
         | FStar_Pervasives_Native.Some tos21 ->
             let uu___ =
               FStar_List_Tot_Base.splitAt (Prims.of_int (3)) datastack in
             (match uu___ with
              | (uu___1, rest_stack) -> tos11 :: tos21 :: tos :: rest_stack))

let (rot_four : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let tos1 = FStar_List_Tot_Base.nth datastack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None -> (Utils.undefinedBehavior "rot_four_1")
        :: datastack
    | FStar_Pervasives_Native.Some tos11 ->
        let tos2 = FStar_List_Tot_Base.nth datastack (Prims.of_int (2)) in
        (match tos2 with
         | FStar_Pervasives_Native.None ->
             (Utils.undefinedBehavior "rot_four_2") :: datastack
         | FStar_Pervasives_Native.Some tos21 ->
             let tos3 = FStar_List_Tot_Base.nth datastack (Prims.of_int (3)) in
             (match tos3 with
              | FStar_Pervasives_Native.None ->
                  (Utils.undefinedBehavior "rot_four_3") :: datastack
              | FStar_Pervasives_Native.Some tos31 ->
                  let uu___ =
                    FStar_List_Tot_Base.splitAt (Prims.of_int (4)) datastack in
                  (match uu___ with
                   | (uu___1, rest_stack) -> tos11 :: tos21 :: tos31 :: tos
                       :: rest_stack)))

let (dup_top : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack -> (FStar_List_Tot_Base.hd datastack) :: datastack

let (dup_top_two : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let tos1 = FStar_List_Tot_Base.nth datastack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None -> (Utils.undefinedBehavior "dup_top") ::
        datastack
    | FStar_Pervasives_Native.Some tos11 -> tos :: tos11 :: datastack

let (unary_positive : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let newDataStack = FStar_List_Tot_Base.tail datastack in
    match tos with
    | Structs.PYTYP obj ->
        (match FStar_Map.sel obj.Structs.methods "__pos__" with
         | Structs.UNFUNBLT f -> (Structs.PYTYP (builtinsToPyObj (f obj))) ::
             newDataStack
         | Structs.ERR s ->
             (Structs.PYTYP
                (PyException.createException "__pos__ is not defined"))
             :: newDataStack
         | uu___ -> (Utils.undefinedBehavior "unary_positive_1") ::
             newDataStack)
    | uu___ -> (Utils.undefinedBehavior "unary_positive_2") :: newDataStack
let (unary_negative : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let newDataStack = FStar_List_Tot_Base.tail datastack in
    match tos with
    | Structs.PYTYP obj ->
        (match FStar_Map.sel obj.Structs.methods "__neg__" with
         | Structs.UNFUNBLT f -> (Structs.PYTYP (builtinsToPyObj (f obj))) ::
             newDataStack
         | Structs.ERR s ->
             (Structs.PYTYP
                (PyException.createException "__neg__ is not defined"))
             :: newDataStack
         | uu___ -> (Utils.undefinedBehavior "unary_negative_1") ::
             newDataStack)
    | uu___ -> (Utils.undefinedBehavior "unary_negative_2") :: newDataStack
let (unary_not : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let newDataStack = FStar_List_Tot_Base.tail datastack in
    match tos with
    | Structs.PYTYP obj ->
        let res =
          match obj.Structs.value with
          | Structs.INT i -> if i = Prims.int_zero then true else false
          | Structs.STRING s -> if s = "" then true else false
          | Structs.BOOL b -> if b then false else true
          | Structs.LIST l -> (match l with | [] -> true | uu___ -> false)
          | Structs.TUPLE t -> (match t with | [] -> true | uu___ -> false)
          | Structs.DICT kvl ->
              (match kvl with | [] -> true | uu___ -> false)
          | Structs.FUNCTION f -> false
          | Structs.EXCEPTION s -> false
          | Structs.USERDEF -> false
          | Structs.NONE -> true in
        (Structs.PYTYP (PyBool.createBool res)) :: newDataStack
    | uu___ -> (Utils.undefinedBehavior "unary_not") :: newDataStack
let (get_iter : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun datastack ->
    let tos = FStar_List_Tot_Base.hd datastack in
    let newDataStack = FStar_List_Tot_Base.tail datastack in
    match tos with
    | Structs.PYTYP obj ->
        (match FStar_Map.sel obj.Structs.methods "__iter__" with
         | Structs.UNFUNOBJ f -> (Structs.PYTYP (f obj)) :: newDataStack
         | Structs.ERR s ->
             (Structs.PYTYP
                (PyException.createException "__iter__ is not defined"))
             :: newDataStack
         | uu___ -> (Utils.undefinedBehavior "get_iter_1") :: newDataStack)
    | uu___ -> (Utils.undefinedBehavior "get_iter_2") :: newDataStack
let (binary_multiply : Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "binary_multiply_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos, tos11) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__mul__" with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__mul__ is not defined"))
                       :: newDataStack
                   | uu___2 -> (Utils.undefinedBehavior "binary_multiply_2")
                       :: newDataStack)
              | (uu___2, uu___3) ->
                  (Utils.undefinedBehavior "binary_multiply_3") ::
                  newDataStack))
let (binary_floor_divide :
  Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "binary_floor_divide_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos, tos11) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__floordiv__"
                   with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__floordiv__ is not defined"))
                       :: newDataStack
                   | uu___2 ->
                       (Utils.undefinedBehavior "binary_floor_divide_2") ::
                       newDataStack)
              | (uu___2, uu___3) ->
                  (Utils.undefinedBehavior "binary_floor_divide_3") ::
                  newDataStack))
let (binary_modulo : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "bianry_modulo_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos, tos11) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__mod__" with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__mod__ is not defined"))
                       :: newDataStack
                   | uu___2 -> (Utils.undefinedBehavior "binary_modulo_2") ::
                       newDataStack)
              | (uu___2, uu___3) ->
                  (Utils.undefinedBehavior "binary_modulo_3") :: newDataStack))
let (binary_add : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "binary_add_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos, tos11) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__add__" with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__add__ is not defined"))
                       :: newDataStack
                   | uu___2 -> (Utils.undefinedBehavior "binary_add_2") ::
                       newDataStack)
              | (uu___2, uu___3) -> (Utils.undefinedBehavior "binary_add_3")
                  :: newDataStack))
let (binary_subtract : Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "binary_subtract_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos, tos11) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__sub__" with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__sub__ is not defined"))
                       :: newDataStack
                   | uu___2 -> (Utils.undefinedBehavior "binary_subtract_2")
                       :: newDataStack)
              | (uu___2, uu___3) ->
                  (Utils.undefinedBehavior "binary_subtract_3") ::
                  newDataStack))
let (binary_subscr : Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun dataStack ->
    let tos = FStar_List_Tot_Base.hd dataStack in
    let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
    match tos1 with
    | FStar_Pervasives_Native.None ->
        (Utils.undefinedBehavior "binary_subscr_1") :: dataStack
    | FStar_Pervasives_Native.Some tos11 ->
        let uu___ = FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
        (match uu___ with
         | (uu___1, newDataStack) ->
             (match (tos11, tos) with
              | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                  (match FStar_Map.sel obj1.Structs.methods "__subscr__" with
                   | Structs.BINFUNBLT f ->
                       (Structs.PYTYP (builtinsToPyObj (f (obj1, obj2)))) ::
                       newDataStack
                   | Structs.ERR s ->
                       (Structs.PYTYP
                          (PyException.createException
                             "__subscr__ is not defined"))
                       :: newDataStack
                   | uu___2 -> (Utils.undefinedBehavior "binary_subscr_2") ::
                       newDataStack)
              | (uu___2, uu___3) ->
                  (Utils.undefinedBehavior "binary_subscr_3") :: newDataStack))
let (build_tuple :
  Prims.nat -> Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun i ->
    fun dataStack ->
      let uu___ = FStar_List_Tot_Base.splitAt i dataStack in
      match uu___ with
      | (elems, newDataStack) ->
          (match Utils.unwrapPyObjList elems with
           | FStar_Pervasives_Native.None ->
               (Utils.undefinedBehavior "build_tuple") :: dataStack
           | FStar_Pervasives_Native.Some l ->
               (Structs.PYTYP (PyTuple.createTuple l)) :: newDataStack)
let (build_list :
  Prims.nat -> Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun i ->
    fun dataStack ->
      let uu___ = FStar_List_Tot_Base.splitAt i dataStack in
      match uu___ with
      | (elems, newDataStack) ->
          (match Utils.unwrapPyObjList elems with
           | FStar_Pervasives_Native.None ->
               (Utils.undefinedBehavior "build_list") :: dataStack
           | FStar_Pervasives_Native.Some l ->
               (Structs.PYTYP (PyList.createList l)) :: newDataStack)
let (build_map :
  Prims.nat -> Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun count ->
    fun dataStack ->
      let uu___ =
        FStar_List_Tot_Base.splitAt ((Prims.of_int (2)) * count) dataStack in
      match uu___ with
      | (elems, newDataStack) ->
          (match Utils.unwrapPyObjList elems with
           | FStar_Pervasives_Native.None ->
               (Utils.undefinedBehavior "build_map_1") :: dataStack
           | FStar_Pervasives_Native.Some clsElems ->
               if
                 ((FStar_List_Tot_Base.length clsElems) mod
                    (Prims.of_int (2)))
                   = Prims.int_zero
               then
                 let vkl = Utils.listToPairs clsElems in
                 let newDataStack1 = (Structs.PYTYP (PyDict.createDict vkl))
                   :: newDataStack in
                 newDataStack1
               else (Utils.undefinedBehavior "build_map_2") :: dataStack)
let (build_const_key_map :
  Prims.nat -> Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun count ->
    fun dataStack ->
      let uu___ = FStar_List_Tot_Base.splitAt Prims.int_one dataStack in
      match uu___ with
      | (keysObj, newDataStack) ->
          (match keysObj with
           | (Structs.PYTYP obj)::[] ->
               (match obj.Structs.value with
                | Structs.TUPLE keysCls ->
                    let uu___1 =
                      FStar_List_Tot_Base.splitAt count newDataStack in
                    (match uu___1 with
                     | (vals, newDataStack1) ->
                         (match Utils.unwrapPyObjList vals with
                          | FStar_Pervasives_Native.None ->
                              (Utils.undefinedBehavior
                                 "build_const_key_map_2")
                              :: newDataStack1
                          | FStar_Pervasives_Native.Some valsCls ->
                              if
                                (FStar_List_Tot_Base.length valsCls) =
                                  (FStar_List_Tot_Base.length keysCls)
                              then
                                let vkl = Utils.totZip valsCls keysCls in
                                let newDataStack2 =
                                  (Structs.PYTYP (PyDict.createDict vkl)) ::
                                  newDataStack1 in
                                newDataStack2
                              else
                                (Utils.undefinedBehavior
                                   "build_const_key_map")
                                :: dataStack))
                | uu___1 -> (Utils.undefinedBehavior "build_const_key_map_2")
                    :: newDataStack)
           | uu___1 -> (Utils.undefinedBehavior "build_const_key_map_3") ::
               newDataStack)
let (compare_op :
  Prims.nat -> Structs.pyObj Prims.list -> Structs.pyObj Prims.list) =
  fun i ->
    fun dataStack ->
      let tos = FStar_List_Tot_Base.hd dataStack in
      let tos1 = FStar_List_Tot_Base.nth dataStack Prims.int_one in
      match tos1 with
      | FStar_Pervasives_Native.None ->
          (Utils.undefinedBehavior "compare_op_3") :: dataStack
      | FStar_Pervasives_Native.Some tos11 ->
          let uu___ =
            FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
          (match uu___ with
           | (uu___1, newDataStack) ->
               let op =
                 match i with
                 | uu___2 when uu___2 = Prims.int_zero -> "__lt__"
                 | uu___2 when uu___2 = Prims.int_one -> "__le__"
                 | uu___2 when uu___2 = (Prims.of_int (2)) -> "__eq__"
                 | uu___2 when uu___2 = (Prims.of_int (3)) -> "__ne__"
                 | uu___2 when uu___2 = (Prims.of_int (4)) -> "__gt__"
                 | uu___2 when uu___2 = (Prims.of_int (5)) -> "__ge__"
                 | uu___2 when uu___2 = (Prims.of_int (6)) -> "__contains__"
                 | uu___2 when uu___2 = (Prims.of_int (7)) -> "__contains__"
                 | uu___2 when uu___2 = (Prims.of_int (8)) -> "__is__"
                 | uu___2 when uu___2 = (Prims.of_int (9)) -> "__nis__"
                 | uu___2 -> "error" in
               (match op with
                | "__eq__" ->
                    (match (tos11, tos) with
                     | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                         (match FStar_Map.sel obj1.Structs.methods op with
                          | Structs.BINFUNBLT f ->
                              (match f (obj1, obj2) with
                               | Structs.BOOL b ->
                                   (Structs.PYTYP (PyBool.createBool b)) ::
                                   newDataStack
                               | uu___2 ->
                                   (Structs.PYTYP
                                      (PyBool.createBool
                                         (obj1.Structs.pid = obj2.Structs.pid)))
                                   :: newDataStack)
                          | uu___2 ->
                              (Utils.undefinedBehavior "compare_op_eq_1") ::
                              newDataStack)
                     | (uu___2, uu___3) ->
                         (Utils.undefinedBehavior "compare_op_eq_2") ::
                         newDataStack)
                | "__ne__" ->
                    (match (tos11, tos) with
                     | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                         (match FStar_Map.sel obj1.Structs.methods op with
                          | Structs.BINFUNBLT f ->
                              (match f (obj1, obj2) with
                               | Structs.BOOL b ->
                                   (Structs.PYTYP (PyBool.createBool b)) ::
                                   newDataStack
                               | uu___2 ->
                                   (Structs.PYTYP
                                      (PyBool.createBool
                                         (obj1.Structs.pid <>
                                            obj2.Structs.pid)))
                                   :: newDataStack)
                          | uu___2 ->
                              (Utils.undefinedBehavior "compare_op_ne_1") ::
                              newDataStack)
                     | (uu___2, uu___3) ->
                         (Utils.undefinedBehavior "compare_op_ne_2") ::
                         newDataStack)
                | "__is__" ->
                    (match (tos11, tos) with
                     | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                         (Structs.PYTYP
                            (PyBool.createBool
                               (obj1.Structs.pid = obj2.Structs.pid)))
                         :: newDataStack
                     | uu___2 -> (Utils.undefinedBehavior "compare_op_is") ::
                         newDataStack)
                | "__nis__" ->
                    (match (tos11, tos) with
                     | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                         (Structs.PYTYP
                            (PyBool.createBool
                               (obj1.Structs.pid <> obj2.Structs.pid)))
                         :: newDataStack
                     | uu___2 -> (Utils.undefinedBehavior "compare_op_nis")
                         :: newDataStack)
                | "error" -> (Utils.undefinedBehavior "compare_op_error") ::
                    newDataStack
                | uu___2 ->
                    (match (tos11, tos) with
                     | (Structs.PYTYP obj1, Structs.PYTYP obj2) ->
                         (match FStar_Map.sel obj1.Structs.methods op with
                          | Structs.BINFUNBLT f ->
                              (Structs.PYTYP
                                 (builtinsToPyObj (f (obj1, obj2))))
                              :: newDataStack
                          | Structs.ERR s ->
                              (Structs.PYTYP
                                 (PyException.createException
                                    "__pos__ is not defined"))
                              :: newDataStack
                          | uu___3 ->
                              (Utils.undefinedBehavior "compare_op_2") ::
                              newDataStack)
                     | (uu___3, uu___4) ->
                         (Utils.undefinedBehavior "compare_op_3") ::
                         newDataStack)))
let (store_name :
  Prims.nat ->
    Prims.string Prims.list ->
      (Prims.string, Structs.pyObj) FStar_Map.t ->
        Structs.pyObj Prims.list ->
          ((Prims.string, Structs.pyObj) FStar_Map.t * Structs.pyObj
            Prims.list))
  =
  fun i ->
    fun names ->
      fun f_locals ->
        fun dataStack ->
          let tos = FStar_List_Tot_Base.hd dataStack in
          let name = FStar_List_Tot_Base.nth names i in
          match name with
          | FStar_Pervasives_Native.None ->
              (f_locals, ((Utils.undefinedBehavior "store_name") ::
                dataStack))
          | FStar_Pervasives_Native.Some name1 ->
              let uu___ = FStar_List_Tot_Base.splitAt Prims.int_one dataStack in
              (match uu___ with
               | (uu___1, newDataStack) ->
                   let newLocals = FStar_Map.upd f_locals name1 tos in
                   (newLocals, newDataStack))
let (load_const :
  Prims.nat ->
    Structs.pyObj Prims.list ->
      Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun i ->
    fun consts ->
      fun dataStack ->
        let elem = FStar_List_Tot_Base.nth consts i in
        match elem with
        | FStar_Pervasives_Native.None ->
            (Utils.undefinedBehavior "load_constant") :: dataStack
        | FStar_Pervasives_Native.Some elem1 ->
            let newDataStack = elem1 :: dataStack in newDataStack
let (load_name :
  Prims.nat ->
    Prims.string Prims.list ->
      (Prims.string, Structs.pyObj) FStar_Map.t ->
        (Prims.string, Structs.pyObj) FStar_Map.t ->
          Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun i ->
    fun names ->
      fun f_locals ->
        fun f_globals ->
          fun dataStack ->
            let name = FStar_List_Tot_Base.nth names i in
            match name with
            | FStar_Pervasives_Native.None ->
                (Utils.undefinedBehavior "load_name") :: dataStack
            | FStar_Pervasives_Native.Some name1 ->
                if FStar_Map.contains f_locals name1
                then (FStar_Map.sel f_locals name1) :: dataStack
                else
                  if FStar_Map.contains f_globals name1
                  then (FStar_Map.sel f_globals name1) :: dataStack
                  else
                    (Structs.PYTYP
                       (PyException.createException
                          (Prims.strcat "name: "
                             (Prims.strcat name1 "is not defined"))))
                    :: dataStack
let (pop_jump_if_true :
  Prims.nat ->
    Prims.nat ->
      Structs.pyObj Prims.list -> (Prims.nat * Structs.pyObj Prims.list))
  =
  fun i ->
    fun pc ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        match tos with
        | Structs.PYTYP obj ->
            (match obj.Structs.value with
             | Structs.BOOL b ->
                 if b
                 then ((i / (Prims.of_int (2))), newDataStack)
                 else (pc, newDataStack)
             | uu___ ->
                 (pc,
                   ((Structs.PYTYP
                       (PyException.createException
                          "ERR: argument is not a Bool")) :: newDataStack)))
        | uu___ ->
            (pc, ((Utils.undefinedBehavior "pop_jump_if_true") ::
              newDataStack))
let (pop_jump_if_false :
  Prims.nat ->
    Prims.nat ->
      Structs.pyObj Prims.list -> (Prims.nat * Structs.pyObj Prims.list))
  =
  fun i ->
    fun pc ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        match tos with
        | Structs.PYTYP obj ->
            (match obj.Structs.value with
             | Structs.BOOL b ->
                 if b
                 then (pc, newDataStack)
                 else ((i / (Prims.of_int (2))), newDataStack)
             | uu___ ->
                 (pc,
                   ((Structs.PYTYP
                       (PyException.createException
                          "ERR: argument is not a Bool")) :: newDataStack)))
        | uu___ ->
            (pc, ((Utils.undefinedBehavior "pop_jump_if_false") ::
              newDataStack))
let (jump_if_true_or_pop :
  Prims.nat ->
    Prims.nat ->
      Structs.pyObj Prims.list -> (Prims.nat * Structs.pyObj Prims.list))
  =
  fun i ->
    fun pc ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        match tos with
        | Structs.PYTYP obj ->
            (match obj.Structs.value with
             | Structs.BOOL b ->
                 if b
                 then ((i / (Prims.of_int (2))), dataStack)
                 else (pc, newDataStack)
             | uu___ ->
                 (pc,
                   ((Structs.PYTYP
                       (PyException.createException
                          "ERR: argument is not a Bool")) :: newDataStack)))
        | uu___ ->
            (pc, ((Utils.undefinedBehavior "jump_if_true_or_pop") ::
              newDataStack))
let (jump_if_false_or_pop :
  Prims.nat ->
    Prims.nat ->
      Structs.pyObj Prims.list -> (Prims.nat * Structs.pyObj Prims.list))
  =
  fun i ->
    fun pc ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        match tos with
        | Structs.PYTYP obj ->
            (match obj.Structs.value with
             | Structs.BOOL b ->
                 if b
                 then (pc, newDataStack)
                 else ((i / (Prims.of_int (2))), dataStack)
             | uu___ ->
                 (pc,
                   ((Structs.PYTYP
                       (PyException.createException
                          "ERR: argument is not a Bool")) :: newDataStack)))
        | uu___ ->
            (pc, ((Utils.undefinedBehavior "jump_if_false_or_pop") ::
              newDataStack))
let (for_iter :
  Prims.nat ->
    Prims.nat ->
      Structs.pyObj Prims.list -> (Prims.nat * Structs.pyObj Prims.list))
  =
  fun i ->
    fun pc ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        match tos with
        | Structs.PYTYP obj ->
            (match FStar_Map.sel obj.Structs.methods "__next__" with
             | Structs.UNFUNBLT f ->
                 (match f obj with
                  | Structs.TUPLE (x::newIter::[]) ->
                      (pc, ((Structs.PYTYP x) :: (Structs.PYTYP newIter) ::
                        newDataStack))
                  | Structs.EXCEPTION "StopIteration" ->
                      ((pc + i), newDataStack)
                  | uu___ ->
                      (pc, ((Utils.undefinedBehavior "for_iter_1") ::
                        newDataStack)))
             | Structs.ERR s ->
                 (pc,
                   ((Structs.PYTYP
                       (PyException.createException "Not iteratable object"))
                   :: newDataStack))
             | uu___ ->
                 (pc, ((Utils.undefinedBehavior "for_iter_2") ::
                   newDataStack)))
        | uu___ ->
            (pc, ((Utils.undefinedBehavior "for_iter_3") :: newDataStack))
let (load_global :
  Prims.nat ->
    Prims.string Prims.list ->
      (Prims.string, Structs.pyObj) FStar_Map.t ->
        Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun i ->
    fun names ->
      fun f_globals ->
        fun dataStack ->
          let name = FStar_List_Tot_Base.nth names i in
          match name with
          | FStar_Pervasives_Native.None ->
              (Utils.undefinedBehavior "load_global") :: dataStack
          | FStar_Pervasives_Native.Some name1 ->
              if FStar_Map.contains f_globals name1
              then (FStar_Map.sel f_globals name1) :: dataStack
              else
                (Structs.PYTYP
                   (PyException.createException
                      (Prims.strcat "name: "
                         (Prims.strcat name1 "is not defined"))))
                :: dataStack
let (load_fast :
  Prims.nat ->
    Structs.pyObj Prims.list ->
      Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun i ->
    fun localPlus ->
      fun dataStack ->
        let elem = FStar_List_Tot_Base.nth localPlus i in
        match elem with
        | FStar_Pervasives_Native.None ->
            (Utils.undefinedBehavior "load_fast") :: dataStack
        | FStar_Pervasives_Native.Some elem1 ->
            let newDataStack = elem1 :: dataStack in newDataStack
let (store_fast :
  Prims.nat ->
    Structs.pyObj Prims.list ->
      Structs.pyObj Prims.list ->
        (Structs.pyObj Prims.list * Structs.pyObj Prims.list))
  =
  fun i ->
    fun localplus ->
      fun dataStack ->
        let tos = FStar_List_Tot_Base.hd dataStack in
        let newDataStack = FStar_List_Tot_Base.tail dataStack in
        if (FStar_List_Tot_Base.length localplus) > i
        then
          let newLocalPlus =
            FStar_List_Tot_Base.mapi
              (fun k -> fun x -> if k = i then tos else x) localplus in
          (newLocalPlus, newDataStack)
        else
          (let newLocalPlus = FStar_List_Tot_Base.append localplus [tos] in
           (newLocalPlus, newDataStack))
let (make_function :
  Prims.nat ->
    (Prims.string, Structs.pyObj) FStar_Map.t ->
      Structs.pyObj Prims.list -> Structs.pyObj Prims.list)
  =
  fun flags ->
    fun globs ->
      fun dataStack ->
        let qualname = FStar_List_Tot_Base.hd dataStack in
        let codeobj = FStar_List_Tot_Base.nth dataStack Prims.int_one in
        match codeobj with
        | FStar_Pervasives_Native.None ->
            (Utils.undefinedBehavior "make_function_1") :: dataStack
        | FStar_Pervasives_Native.Some codeobj1 ->
            let uu___ =
              FStar_List_Tot_Base.splitAt (Prims.of_int (2)) dataStack in
            (match uu___ with
             | (uu___1, newDataStack) ->
                 (match newDataStack with
                  | [] -> (Utils.undefinedBehavior "make_function_2") ::
                      dataStack
                  | uu___2 ->
                      let func =
                        PyFunction.createFunction
                          {
                            Structs.func_Code = codeobj1;
                            Structs.func_globals = globs;
                            Structs.func_name = qualname;
                            Structs.func_closure =
                              (if flags = (Prims.of_int (8))
                               then
                                 match FStar_List_Tot_Base.hd newDataStack
                                 with
                                 | Structs.PYTYP obj ->
                                     (match obj.Structs.value with
                                      | Structs.TUPLE t ->
                                          Structs.PYTYP
                                            (PyTuple.createTuple t)
                                      | uu___3 ->
                                          Utils.undefinedBehavior
                                            "make_function_func_closure_1")
                                 | uu___3 ->
                                     Utils.undefinedBehavior
                                       "make_function_func_closure_2"
                               else Structs.PYTYP (PyNone.createNone ()));
                            Structs.func_defaults =
                              (if flags = Prims.int_one
                               then
                                 match FStar_List_Tot_Base.hd newDataStack
                                 with
                                 | Structs.PYTYP obj ->
                                     (match obj.Structs.value with
                                      | Structs.TUPLE t ->
                                          Structs.PYTYP
                                            (PyTuple.createTuple t)
                                      | uu___3 ->
                                          Utils.undefinedBehavior
                                            "make_function_func_defaults_1")
                                 | uu___3 ->
                                     Utils.undefinedBehavior
                                       "make_function_func_defaults_2"
                               else Structs.PYTYP (PyNone.createNone ()))
                          } in
                      (match flags with
                       | uu___3 when uu___3 = Prims.int_zero ->
                           (Structs.PYTYP func) :: newDataStack
                       | uu___3 -> (Structs.PYTYP func) ::
                           (FStar_List_Tot_Base.tail newDataStack))))
let rec (execBytecode : Structs.frameObj -> Structs.frameObj) =
  fun frame ->
    let uu___ = Utils.check_err frame.Structs.dataStack in
    match uu___ with
    | FStar_Pervasives_Native.Some err -> frame
    | FStar_Pervasives_Native.None ->
        let uu___1 = (frame.Structs.fCode).Structs.co_code in
        (match uu___1 with
         | Structs.CODE bc ->
             let uu___2 = FStar_List.nth bc frame.Structs.pc in
             (match uu___2 with
              | Structs.RETURN_VALUE -> frame
              | Structs.CALL_FUNCTION i ->
                  if (FStar_List_Tot_Base.length frame.Structs.dataStack) > i
                  then
                    let newDataStack =
                      call_function i frame.Structs.f_globals
                        frame.Structs.dataStack frame.Structs.f_idCount
                        frame.Structs.f_usedIds in
                    {
                      Structs.dataStack = newDataStack;
                      Structs.blockStack = (frame.Structs.blockStack);
                      Structs.fCode = (frame.Structs.fCode);
                      Structs.pc = (frame.Structs.pc);
                      Structs.f_localplus = (frame.Structs.f_localplus);
                      Structs.f_globals = (frame.Structs.f_globals);
                      Structs.f_locals = (frame.Structs.f_locals);
                      Structs.f_idCount = (frame.Structs.f_idCount);
                      Structs.f_usedIds = (frame.Structs.f_usedIds)
                    }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "CALL_FUNCTION"] in
                     {
                       Structs.dataStack = newDataStack;
                       Structs.blockStack = (frame.Structs.blockStack);
                       Structs.fCode = (frame.Structs.fCode);
                       Structs.pc = (frame.Structs.pc);
                       Structs.f_localplus = (frame.Structs.f_localplus);
                       Structs.f_globals = (frame.Structs.f_globals);
                       Structs.f_locals = (frame.Structs.f_locals);
                       Structs.f_idCount = (frame.Structs.f_idCount);
                       Structs.f_usedIds = (frame.Structs.f_usedIds)
                     })
              | Structs.NOP ->
                  execBytecode
                    {
                      Structs.dataStack = (frame.Structs.dataStack);
                      Structs.blockStack = (frame.Structs.blockStack);
                      Structs.fCode = (frame.Structs.fCode);
                      Structs.pc = (frame.Structs.pc + Prims.int_one);
                      Structs.f_localplus = (frame.Structs.f_localplus);
                      Structs.f_globals = (frame.Structs.f_globals);
                      Structs.f_locals = (frame.Structs.f_locals);
                      Structs.f_idCount = (frame.Structs.f_idCount);
                      Structs.f_usedIds = (frame.Structs.f_usedIds)
                    }
              | Structs.POP_TOP ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack = [Utils.undefinedBehavior "POP_TOP"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack = pop_top frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.ROT_TWO ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack = rot_two frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "ROT_TWO"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.ROT_THREE ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (3))
                  then
                    let newDataStack = rot_three frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "ROT_THREE"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.ROT_FOUR ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (4))
                  then
                    let newDataStack = rot_four frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "ROT_FOUR"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.DUP_TOP ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack = [Utils.undefinedBehavior "DUP_TOP"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack = dup_top frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.DUP_TOP_TWO ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack = dup_top_two frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "DUP_TOP_TWO"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.UNARY_POSITIVE ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "UNARY_POSITIVE"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack =
                         unary_positive frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.UNARY_NEGATIVE ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "UNARY_NEGATIVE"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack =
                         unary_negative frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.UNARY_NOT ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "UNARY_NOT"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack = unary_not frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.GET_ITER ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "GET_ITER"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let newDataStack = get_iter frame.Structs.dataStack in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         })
              | Structs.BINARY_MULTIPLY ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack =
                      binary_multiply frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_MULTIPLY"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BINARY_FLOOR_DIVIDE ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack =
                      binary_floor_divide frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_FLOOR_DIVIDE"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BINARY_MODULO ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack = binary_modulo frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_MODULO"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BINARY_ADD ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack = binary_add frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_ADD"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BINARY_SUBTRACT ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack =
                      binary_subtract frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_SUBTRACT"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BINARY_SUBSCR ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack =
                      binary_subtract frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BINARY_SUBSCR"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.LOAD_CONST i ->
                  if
                    (FStar_List_Tot_Base.length
                       (frame.Structs.fCode).Structs.co_consts)
                      > i
                  then
                    let newDataStack =
                      load_const i (frame.Structs.fCode).Structs.co_consts
                        frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "LOAD_CONST"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.LOAD_NAME i ->
                  if
                    (FStar_List_Tot_Base.length
                       (frame.Structs.fCode).Structs.co_names)
                      > i
                  then
                    let newDataStack =
                      load_name i (frame.Structs.fCode).Structs.co_names
                        frame.Structs.f_locals frame.Structs.f_globals
                        frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "LOAD_NAME"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BUILD_TUPLE i ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >= i
                  then
                    let newDataStack = build_tuple i frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BUILD_TUPLE"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.STORE_NAME i ->
                  if
                    ((FStar_List_Tot_Base.length
                        (frame.Structs.fCode).Structs.co_names)
                       > i)
                      &&
                      ((FStar_List_Tot_Base.length frame.Structs.dataStack)
                         >= Prims.int_one)
                  then
                    let uu___3 =
                      store_name i (frame.Structs.fCode).Structs.co_names
                        frame.Structs.f_locals frame.Structs.dataStack in
                    (match uu___3 with
                     | (newLocals, newDataStack) ->
                         execBytecode
                           {
                             Structs.dataStack = newDataStack;
                             Structs.blockStack = (frame.Structs.blockStack);
                             Structs.fCode = (frame.Structs.fCode);
                             Structs.pc = (frame.Structs.pc + Prims.int_one);
                             Structs.f_localplus =
                               (frame.Structs.f_localplus);
                             Structs.f_globals = (frame.Structs.f_globals);
                             Structs.f_locals = newLocals;
                             Structs.f_idCount = (frame.Structs.f_idCount);
                             Structs.f_usedIds = (frame.Structs.f_usedIds)
                           })
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "STORE_NAME"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BUILD_LIST i ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >= i
                  then
                    let newDataStack = build_list i frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BUILD_LIST"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BUILD_MAP i ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      ((Prims.of_int (2)) * i)
                  then
                    let newDataStack = build_map i frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "BUILD_MAP"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.BUILD_CONST_KEY_MAP i ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (i + Prims.int_one)
                  then
                    let newDataStack =
                      build_const_key_map i frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "BUILD_CONST_KEY_MAP"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.JUMP_FORWARD i ->
                  execBytecode
                    {
                      Structs.dataStack = (frame.Structs.dataStack);
                      Structs.blockStack = (frame.Structs.blockStack);
                      Structs.fCode = (frame.Structs.fCode);
                      Structs.pc =
                        ((frame.Structs.pc + (i / (Prims.of_int (2)))) +
                           Prims.int_one);
                      Structs.f_localplus = (frame.Structs.f_localplus);
                      Structs.f_globals = (frame.Structs.f_globals);
                      Structs.f_locals = (frame.Structs.f_locals);
                      Structs.f_idCount = (frame.Structs.f_idCount);
                      Structs.f_usedIds = (frame.Structs.f_usedIds)
                    }
              | Structs.POP_JUMP_IF_TRUE i ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "POP_JUMP_IF_TRUE"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let uu___4 =
                         pop_jump_if_true i frame.Structs.pc
                           frame.Structs.dataStack in
                       (match uu___4 with
                        | (newPc, newDataStack) ->
                            let newPc1 =
                              if frame.Structs.pc = newPc
                              then newPc + Prims.int_one
                              else newPc in
                            execBytecode
                              {
                                Structs.dataStack = newDataStack;
                                Structs.blockStack =
                                  (frame.Structs.blockStack);
                                Structs.fCode = (frame.Structs.fCode);
                                Structs.pc = newPc1;
                                Structs.f_localplus =
                                  (frame.Structs.f_localplus);
                                Structs.f_globals = (frame.Structs.f_globals);
                                Structs.f_locals = (frame.Structs.f_locals);
                                Structs.f_idCount = (frame.Structs.f_idCount);
                                Structs.f_usedIds = (frame.Structs.f_usedIds)
                              }))
              | Structs.POP_JUMP_IF_FALSE i ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "POP_JUMP_IF_FALSE"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let uu___4 =
                         pop_jump_if_false i frame.Structs.pc
                           frame.Structs.dataStack in
                       (match uu___4 with
                        | (newPc, newDataStack) ->
                            let newPc1 =
                              if frame.Structs.pc = newPc
                              then newPc + Prims.int_one
                              else newPc in
                            execBytecode
                              {
                                Structs.dataStack = newDataStack;
                                Structs.blockStack =
                                  (frame.Structs.blockStack);
                                Structs.fCode = (frame.Structs.fCode);
                                Structs.pc = newPc1;
                                Structs.f_localplus =
                                  (frame.Structs.f_localplus);
                                Structs.f_globals = (frame.Structs.f_globals);
                                Structs.f_locals = (frame.Structs.f_locals);
                                Structs.f_idCount = (frame.Structs.f_idCount);
                                Structs.f_usedIds = (frame.Structs.f_usedIds)
                              }))
              | Structs.JUMP_IF_TRUE_OR_POP i ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "JUMP_IF_TRUE_OR_POP"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let uu___4 =
                         jump_if_true_or_pop i frame.Structs.pc
                           frame.Structs.dataStack in
                       (match uu___4 with
                        | (newPc, newDataStack) ->
                            let newPc1 =
                              if frame.Structs.pc = newPc
                              then newPc + Prims.int_one
                              else newPc in
                            execBytecode
                              {
                                Structs.dataStack = newDataStack;
                                Structs.blockStack =
                                  (frame.Structs.blockStack);
                                Structs.fCode = (frame.Structs.fCode);
                                Structs.pc = newPc1;
                                Structs.f_localplus =
                                  (frame.Structs.f_localplus);
                                Structs.f_globals = (frame.Structs.f_globals);
                                Structs.f_locals = (frame.Structs.f_locals);
                                Structs.f_idCount = (frame.Structs.f_idCount);
                                Structs.f_usedIds = (frame.Structs.f_usedIds)
                              }))
              | Structs.JUMP_IF_FALSE_OR_POP i ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "JUMP_IF_FALSE_OR_POP"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let uu___4 =
                         jump_if_false_or_pop i frame.Structs.pc
                           frame.Structs.dataStack in
                       (match uu___4 with
                        | (newPc, newDataStack) ->
                            let newPc1 =
                              if frame.Structs.pc = newPc
                              then newPc + Prims.int_one
                              else newPc in
                            execBytecode
                              {
                                Structs.dataStack = newDataStack;
                                Structs.blockStack =
                                  (frame.Structs.blockStack);
                                Structs.fCode = (frame.Structs.fCode);
                                Structs.pc = newPc1;
                                Structs.f_localplus =
                                  (frame.Structs.f_localplus);
                                Structs.f_globals = (frame.Structs.f_globals);
                                Structs.f_locals = (frame.Structs.f_locals);
                                Structs.f_idCount = (frame.Structs.f_idCount);
                                Structs.f_usedIds = (frame.Structs.f_usedIds)
                              }))
              | Structs.FOR_ITER i ->
                  (match frame.Structs.dataStack with
                   | [] ->
                       let newDataStack =
                         [Utils.undefinedBehavior "FOR_ITER"] in
                       execBytecode
                         {
                           Structs.dataStack = newDataStack;
                           Structs.blockStack = (frame.Structs.blockStack);
                           Structs.fCode = (frame.Structs.fCode);
                           Structs.pc = (frame.Structs.pc + Prims.int_one);
                           Structs.f_localplus = (frame.Structs.f_localplus);
                           Structs.f_globals = (frame.Structs.f_globals);
                           Structs.f_locals = (frame.Structs.f_locals);
                           Structs.f_idCount = (frame.Structs.f_idCount);
                           Structs.f_usedIds = (frame.Structs.f_usedIds)
                         }
                   | uu___3 ->
                       let uu___4 =
                         for_iter i frame.Structs.pc frame.Structs.dataStack in
                       (match uu___4 with
                        | (newPc, newDataStack) ->
                            let newPc1 =
                              if frame.Structs.pc = newPc
                              then newPc + Prims.int_one
                              else newPc in
                            execBytecode
                              {
                                Structs.dataStack = newDataStack;
                                Structs.blockStack =
                                  (frame.Structs.blockStack);
                                Structs.fCode = (frame.Structs.fCode);
                                Structs.pc = newPc1;
                                Structs.f_localplus =
                                  (frame.Structs.f_localplus);
                                Structs.f_globals = (frame.Structs.f_globals);
                                Structs.f_locals = (frame.Structs.f_locals);
                                Structs.f_idCount = (frame.Structs.f_idCount);
                                Structs.f_usedIds = (frame.Structs.f_usedIds)
                              }))
              | Structs.JUMP_ABSOLUTE i ->
                  execBytecode
                    {
                      Structs.dataStack = (frame.Structs.dataStack);
                      Structs.blockStack = (frame.Structs.blockStack);
                      Structs.fCode = (frame.Structs.fCode);
                      Structs.pc = (i / (Prims.of_int (2)));
                      Structs.f_localplus = (frame.Structs.f_localplus);
                      Structs.f_globals = (frame.Structs.f_globals);
                      Structs.f_locals = (frame.Structs.f_locals);
                      Structs.f_idCount = (frame.Structs.f_idCount);
                      Structs.f_usedIds = (frame.Structs.f_usedIds)
                    }
              | Structs.LOAD_GLOBAL i ->
                  if
                    (FStar_List_Tot_Base.length
                       (frame.Structs.fCode).Structs.co_names)
                      > i
                  then
                    let newDataStack =
                      load_global i (frame.Structs.fCode).Structs.co_names
                        frame.Structs.f_globals frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "LOAD_GLOBAL"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.LOAD_FAST i ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.f_localplus) >
                      i
                  then
                    let newDataStack =
                      load_fast i frame.Structs.f_localplus
                        frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack = [Utils.undefinedBehavior "LOAD_FAST"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.STORE_FAST i ->
                  if
                    ((FStar_List_Tot_Base.length frame.Structs.f_localplus)
                       >= i)
                      &&
                      ((FStar_List_Tot_Base.length frame.Structs.dataStack)
                         >= Prims.int_one)
                  then
                    let uu___3 =
                      store_fast i frame.Structs.f_localplus
                        frame.Structs.dataStack in
                    (match uu___3 with
                     | (newLocalPlus, newDataStack) ->
                         execBytecode
                           {
                             Structs.dataStack = newDataStack;
                             Structs.blockStack = (frame.Structs.blockStack);
                             Structs.fCode = (frame.Structs.fCode);
                             Structs.pc = (frame.Structs.pc + Prims.int_one);
                             Structs.f_localplus = newLocalPlus;
                             Structs.f_globals = (frame.Structs.f_globals);
                             Structs.f_locals = (frame.Structs.f_locals);
                             Structs.f_idCount = (frame.Structs.f_idCount);
                             Structs.f_usedIds = (frame.Structs.f_usedIds)
                           })
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "STORE_FAST"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | Structs.MAKE_FUNCTION flags ->
                  if
                    (FStar_List_Tot_Base.length frame.Structs.dataStack) >=
                      (Prims.of_int (2))
                  then
                    let newDataStack =
                      make_function flags frame.Structs.f_globals
                        frame.Structs.dataStack in
                    execBytecode
                      {
                        Structs.dataStack = newDataStack;
                        Structs.blockStack = (frame.Structs.blockStack);
                        Structs.fCode = (frame.Structs.fCode);
                        Structs.pc = (frame.Structs.pc + Prims.int_one);
                        Structs.f_localplus = (frame.Structs.f_localplus);
                        Structs.f_globals = (frame.Structs.f_globals);
                        Structs.f_locals = (frame.Structs.f_locals);
                        Structs.f_idCount = (frame.Structs.f_idCount);
                        Structs.f_usedIds = (frame.Structs.f_usedIds)
                      }
                  else
                    (let newDataStack =
                       [Utils.undefinedBehavior "MAKE_FUNCTION"] in
                     execBytecode
                       {
                         Structs.dataStack = newDataStack;
                         Structs.blockStack = (frame.Structs.blockStack);
                         Structs.fCode = (frame.Structs.fCode);
                         Structs.pc = (frame.Structs.pc + Prims.int_one);
                         Structs.f_localplus = (frame.Structs.f_localplus);
                         Structs.f_globals = (frame.Structs.f_globals);
                         Structs.f_locals = (frame.Structs.f_locals);
                         Structs.f_idCount = (frame.Structs.f_idCount);
                         Structs.f_usedIds = (frame.Structs.f_usedIds)
                       })
              | uu___3 ->
                  let newDataStack =
                    [Utils.undefinedBehavior "INSTRUCTION_NOT_SUPPORTED"] in
                  execBytecode
                    {
                      Structs.dataStack = newDataStack;
                      Structs.blockStack = (frame.Structs.blockStack);
                      Structs.fCode = (frame.Structs.fCode);
                      Structs.pc = (frame.Structs.pc + Prims.int_one);
                      Structs.f_localplus = (frame.Structs.f_localplus);
                      Structs.f_globals = (frame.Structs.f_globals);
                      Structs.f_locals = (frame.Structs.f_locals);
                      Structs.f_idCount = (frame.Structs.f_idCount);
                      Structs.f_usedIds = (frame.Structs.f_usedIds)
                    }))
let rec (runFrame :
  Structs.vm -> Structs.frameObj -> (Structs.vm * Structs.pyObj)) =
  fun virM ->
    fun frame ->
      let resultFrame = execBytecode frame in
      let result = FStar_List.hd resultFrame.Structs.dataStack in
      match result with
      | Structs.FRAMEOBJECT newFrame ->
          let resultStack =
            let uu___ = FStar_List.tail resultFrame.Structs.dataStack in
            {
              Structs.dataStack = uu___;
              Structs.blockStack = (resultFrame.Structs.blockStack);
              Structs.fCode = (resultFrame.Structs.fCode);
              Structs.pc = (resultFrame.Structs.pc);
              Structs.f_localplus = (resultFrame.Structs.f_localplus);
              Structs.f_globals = (resultFrame.Structs.f_globals);
              Structs.f_locals = (resultFrame.Structs.f_locals);
              Structs.f_idCount = (resultFrame.Structs.f_idCount);
              Structs.f_usedIds = (resultFrame.Structs.f_usedIds)
            } in
          let newVM =
            {
              Structs.callStack = (resultFrame :: (virM.Structs.callStack));
              Structs.code = (virM.Structs.code);
              Structs.vmpid = (virM.Structs.vmpid);
              Structs.idCount = (virM.Structs.idCount);
              Structs.usedIds = (virM.Structs.usedIds)
            } in
          runFrame newVM newFrame
      | uu___ ->
          let new_globals = resultFrame.Structs.f_globals in
          let popVM =
            let uu___1 = FStar_List.tail virM.Structs.callStack in
            {
              Structs.callStack = uu___1;
              Structs.code = (virM.Structs.code);
              Structs.vmpid = (virM.Structs.vmpid);
              Structs.idCount = (virM.Structs.idCount);
              Structs.usedIds = (virM.Structs.usedIds)
            } in
          if
            (FStar_List_Tot_Base.length popVM.Structs.callStack) =
              Prims.int_zero
          then (popVM, result)
          else
            (let callerFrame = FStar_List.hd popVM.Structs.callStack in
             let newCallStack = FStar_List.tail popVM.Structs.callStack in
             let newCallerFrame =
               {
                 Structs.dataStack = (result ::
                   (callerFrame.Structs.dataStack));
                 Structs.blockStack = (callerFrame.Structs.blockStack);
                 Structs.fCode = (callerFrame.Structs.fCode);
                 Structs.pc = (callerFrame.Structs.pc + Prims.int_one);
                 Structs.f_localplus = (callerFrame.Structs.f_localplus);
                 Structs.f_globals = new_globals;
                 Structs.f_locals = (callerFrame.Structs.f_locals);
                 Structs.f_idCount = (callerFrame.Structs.f_idCount);
                 Structs.f_usedIds = (callerFrame.Structs.f_usedIds)
               } in
             let newVM =
               {
                 Structs.callStack = (newCallerFrame :: newCallStack);
                 Structs.code = (popVM.Structs.code);
                 Structs.vmpid = (popVM.Structs.vmpid);
                 Structs.idCount = (popVM.Structs.idCount);
                 Structs.usedIds = (popVM.Structs.usedIds)
               } in
             runFrame newVM newCallerFrame)