open Prims
let (check_err :
  Structs.pyObj Prims.list -> Structs.pyObj FStar_Pervasives_Native.option) =
  fun dataStack ->
    match FStar_List_Tot_Base.length dataStack with
    | uu___ when uu___ = Prims.int_zero -> FStar_Pervasives_Native.None
    | uu___ ->
        let uu___1 = FStar_List.hd dataStack in
        (match uu___1 with
         | Structs.ERR s -> FStar_Pervasives_Native.Some (Structs.ERR s)
         | Structs.PYTYP obj ->
             (match obj.Structs.value with
              | Structs.EXCEPTION e ->
                  FStar_Pervasives_Native.Some (Structs.PYTYP obj)
              | uu___2 -> FStar_Pervasives_Native.None)
         | uu___2 -> FStar_Pervasives_Native.None)
let rec (print_pyObj : Structs.pyObj -> Prims.string) =
  fun p ->
    match p with
    | Structs.ERR s -> Prims.strcat "ERR:" (Prims.strcat s "")
    | Structs.CODEOBJECT co ->
        let uu___ = print_codeObj co in
        Prims.strcat "CODEOBJECT:" (Prims.strcat uu___ "")
    | Structs.FRAMEOBJECT fo ->
        let uu___ = print_codeObj fo.Structs.fCode in
        Prims.strcat "FRAME:" (Prims.strcat uu___ "")
    | Structs.UNFUNOBJ f -> "UNFUNOBJ:"
    | Structs.BINFUNBLT f -> "BINFUNBLT:"
    | Structs.PYTYP typ -> print_type0 typ
    | uu___ -> "UNFUNBLT:"
and (print_type0 : Structs.cls -> Prims.string) =
  fun t ->
    match t.Structs.value with
    | Structs.USERDEF ->
        Prims.strcat "CLASS:" (Prims.strcat t.Structs.name "")
    | uu___ -> print_builtin t.Structs.value
and (print_vk : (Structs.cls * Structs.cls) -> Prims.string) =
  fun vk ->
    match vk with
    | (v, k) ->
        let uu___ = print_builtin k.Structs.value in
        let uu___1 =
          let uu___2 = print_builtin v.Structs.value in
          Prims.strcat ":" uu___2 in
        Prims.strcat uu___ uu___1
and (print_builtin : Structs.builtins -> Prims.string) =
  fun b ->
    match b with
    | Structs.INT i ->
        Prims.strcat "INT:" (Prims.strcat (Prims.string_of_int i) "")
    | Structs.STRING s -> Prims.strcat "STRING:" (Prims.strcat s "")
    | Structs.BOOL b1 ->
        Prims.strcat "BOOL:" (Prims.strcat (Prims.string_of_bool b1) "")
    | Structs.LIST l ->
        let uu___ =
          let uu___1 =
            let uu___2 = FStar_List.map print_type0 l in
            FStar_List.fold_right
              (fun a -> fun b1 -> Prims.strcat a (Prims.strcat "," b1))
              uu___2 "]" in
          Prims.strcat "[" uu___1 in
        Prims.strcat "LIST:" (Prims.strcat uu___ "")
    | Structs.TUPLE l ->
        let uu___ =
          let uu___1 =
            let uu___2 = FStar_List.map print_type0 l in
            FStar_List.fold_right
              (fun a -> fun b1 -> Prims.strcat a (Prims.strcat "," b1))
              uu___2 ")" in
          Prims.strcat "(" uu___1 in
        Prims.strcat "TUPLE:" (Prims.strcat uu___ "")
    | Structs.DICT vkl ->
        let uu___ =
          let uu___1 =
            let uu___2 = FStar_List.map print_vk vkl in
            FStar_List.fold_right
              (fun a -> fun b1 -> Prims.strcat a (Prims.strcat "," b1))
              uu___2 "}" in
          Prims.strcat "{" uu___1 in
        Prims.strcat "DICT:" (Prims.strcat uu___ "")
    | Structs.FUNCTION fo ->
        let uu___ = print_pyObj fo.Structs.func_name in
        Prims.strcat "FUNCTION:" (Prims.strcat uu___ "")
    | Structs.EXCEPTION s -> Prims.strcat "EXCEPTION:" (Prims.strcat s "")
    | Structs.USERDEF -> "USERDEF:"
    | Structs.NONE -> "NONE:None"
and (print_codeObj : Structs.codeObj -> Prims.string) =
  fun co ->
    let constansts_string =
      let uu___ =
        let uu___1 =
          let uu___2 = FStar_List.map print_pyObj co.Structs.co_consts in
          FStar_List.fold_right
            (fun a -> fun b -> Prims.strcat a (Prims.strcat "," b)) uu___2
            "]" in
        Prims.strcat "[" uu___1 in
      Prims.strcat "CONSTANTS:" (Prims.strcat uu___ "") in
    let varnames_string =
      let uu___ =
        let uu___1 =
          FStar_List.fold_right
            (fun a -> fun b -> Prims.strcat a (Prims.strcat "," b))
            co.Structs.co_varnames "]" in
        Prims.strcat "[" uu___1 in
      Prims.strcat "VARNAMES:" (Prims.strcat uu___ "") in
    let names_string =
      let uu___ =
        let uu___1 =
          FStar_List.fold_right
            (fun a -> fun b -> Prims.strcat a (Prims.strcat "," b))
            co.Structs.co_names "]" in
        Prims.strcat "[" uu___1 in
      Prims.strcat "NAMES:" (Prims.strcat uu___ "") in
    Prims.strcat
      (Prims.strcat (Prims.strcat "[" (Prims.strcat constansts_string ", "))
         (Prims.strcat varnames_string ", ")) (Prims.strcat names_string "]")
and (print_program_state : Structs.vm -> Structs.pyObj -> Prims.string) =
  fun state ->
    fun result ->
      let result_string =
        let uu___ = print_pyObj result in
        Prims.strcat "RESULT:" (Prims.strcat uu___ "") in
      let co_string = print_codeObj state.Structs.code in
      Prims.strcat (Prims.strcat "STATE:[" (Prims.strcat result_string ", "))
        (Prims.strcat co_string "]")
let rec (subString_pos' :
  FStar_String.char Prims.list ->
    Prims.int -> FStar_String.char FStar_Pervasives_Native.option)
  =
  fun cl ->
    fun i ->
      match cl with
      | [] -> FStar_Pervasives_Native.None
      | x::cl' ->
          if i = Prims.int_zero
          then FStar_Pervasives_Native.Some x
          else
            if i < Prims.int_zero
            then FStar_Pervasives_Native.None
            else subString_pos' cl' (i - Prims.int_one)
let (subString_pos :
  Prims.string -> Prims.int -> Prims.string FStar_Pervasives_Native.option) =
  fun s ->
    fun i ->
      let uu___ = subString_pos' (FStar_String.list_of_string s) i in
      match uu___ with
      | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
      | FStar_Pervasives_Native.Some c ->
          FStar_Pervasives_Native.Some (FStar_String.string_of_char c)
let (subString_neg :
  Prims.string -> Prims.int -> Prims.string FStar_Pervasives_Native.option) =
  fun s -> fun i -> subString_pos s ((FStar_String.strlen s) + i)
let rec tabulate' : 'a . (Prims.nat -> 'a) -> Prims.nat -> 'a Prims.list =
  fun f ->
    fun i ->
      match i with
      | uu___ when uu___ = Prims.int_zero -> []
      | i1 -> (f (i1 - Prims.int_one)) :: (tabulate' f (i1 - Prims.int_one))
let tabulate : 'a . (Prims.nat -> 'a) -> Prims.nat -> 'a Prims.list =
  fun f -> fun i -> FStar_List_Tot_Base.rev (tabulate' f i)
let rec listToPairs : 'a . 'a Prims.list -> ('a * 'a) Prims.list =
  fun l -> match l with | [] -> [] | x::y::l2 -> (x, y) :: (listToPairs l2)
let (pyObjToobj : Structs.pyObj -> Structs.cls) =
  fun p -> match p with | Structs.PYTYP obj -> obj
let rec (unwrapPyObjList :
  Structs.pyObj Prims.list ->
    Structs.cls Prims.list FStar_Pervasives_Native.option)
  =
  fun l ->
    match l with
    | [] -> FStar_Pervasives_Native.Some []
    | x::l1 ->
        (match x with
         | Structs.PYTYP obj ->
             let l2 = unwrapPyObjList l1 in
             (match l2 with
              | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
              | FStar_Pervasives_Native.Some l3 ->
                  FStar_Pervasives_Native.Some (obj :: l3))
         | uu___ -> FStar_Pervasives_Native.None)
let rec totZip :
  'a 'b . 'a Prims.list -> 'b Prims.list -> ('a * 'b) Prims.list =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> []
      | (hd1::tl1, hd2::tl2) -> (hd1, hd2) :: (totZip tl1 tl2)
let (objEq : Structs.cls -> Structs.cls -> Prims.bool) =
  fun obj1 ->
    fun obj2 ->
      match FStar_Map.sel obj1.Structs.methods "__eq__" with
      | Structs.BINFUNBLT f ->
          (match f (obj1, obj2) with | Structs.BOOL b -> b | uu___ -> false)
      | uu___ -> false
let rec (list_contains : Structs.cls Prims.list -> Structs.cls -> Prims.bool)
  =
  fun l ->
    fun x ->
      match l with
      | [] -> false
      | h::l1 -> (objEq x h) || (list_contains l1 x)
let rec (list_lex_lt :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL false
      | ([], h2::l21) -> Structs.BOOL true
      | (h1::l11, []) -> Structs.BOOL false
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__ge__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_lt l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let rec (list_lex_le :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL true
      | ([], h2::l21) -> Structs.BOOL true
      | (h1::l11, []) -> Structs.BOOL false
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__gt__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_le l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let rec (list_lex_eq :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL true
      | ([], h2::l21) -> Structs.BOOL false
      | (h1::l11, []) -> Structs.BOOL false
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__ne__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_eq l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let rec (list_lex_ne :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL false
      | ([], h2::l21) -> Structs.BOOL true
      | (h1::l11, []) -> Structs.BOOL true
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__eq__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_ne l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let rec (list_lex_gt :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL false
      | ([], h2::l21) -> Structs.BOOL false
      | (h1::l11, []) -> Structs.BOOL true
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__le__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_gt l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let rec (list_lex_ge :
  Structs.cls Prims.list -> Structs.cls Prims.list -> Structs.builtins) =
  fun l1 ->
    fun l2 ->
      match (l1, l2) with
      | ([], []) -> Structs.BOOL true
      | ([], h2::l21) -> Structs.BOOL false
      | (h1::l11, []) -> Structs.BOOL true
      | (h1::l11, h2::l21) ->
          (match FStar_Map.sel h1.Structs.methods "__lt__" with
           | Structs.BINFUNBLT f ->
               (match f (h1, h2) with
                | Structs.NONE -> Structs.NONE
                | Structs.BOOL b ->
                    if b then Structs.BOOL false else list_lex_ge l11 l21
                | uu___ -> Structs.NONE)
           | err -> Structs.NONE)
let (emptyMap : (Prims.string, Structs.pyObj) FStar_Map.t) =
  FStar_Map.const (Structs.ERR "UNDEFINED")
let (undefinedBehavior : Prims.string -> Structs.pyObj) =
  fun s ->
    Structs.ERR
      (FStar_String.concat "" ["INTERPRTER UNDEFINED BEHAVIOR "; s])
let (idsMap : (Structs.hashable, Prims.nat) FStar_Map.t) =
  FStar_Map.const Prims.int_zero