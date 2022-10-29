module Exec

(* imported modules *)
open Structs
open Utils
open PyBuiltinObjects
open FStar.List.Tot.Base
(* ---------------- *)

val builtinsToPyObj: builtins -> Tot cls

val makeFrame: vm -> codeObj -> list pyObj -> (Map.t string pyObj) 
                  -> (Map.t string pyObj) -> Tot (vm * frameObj)

val call_function:  (i:nat)
                 -> (l:list pyObj {length l > i}) 
                 -> nat
                 -> Map.t hashable nat -> Tot (list pyObj)

val pop_top: (l:list pyObj {Cons? l}) -> Tot (l2:list pyObj {l2 == tail l})

val rot_two: (l:list pyObj {length l >= 2})
             -> Tot (l2:list pyObj 
                        {l2 == (hd (tail l))::(hd l)::(tail (tail l))})

val rot_three: (l:list pyObj {length l >= 3})
               -> Tot (l2:list pyObj 
                          {l2 == (hd (tail l))::
                                 (hd (tail (tail l)))::
                                 (hd l)::
                                 (tail (tail (tail l)))})

val rot_four: (l:list pyObj {length l >= 4})
              -> Tot (l2:list pyObj
                         {l2 == (hd (tail l))::
                                (hd (tail (tail l)))::
                                (hd (tail (tail (tail l))))::
                                (hd l)::
                                (tail (tail (tail (tail l))))})

val dup_top: (l:list pyObj {Cons? l}) -> Tot (l2:list pyObj {l2 == (hd l)::l})

val dup_top_two: (l:list pyObj {length l >= 2})
                 -> Tot (l2:list pyObj {l2 == (hd l)::(hd (tail l))::l})

val unary_positive: (l:list pyObj {Cons? l}) -> Tot (list pyObj)

val unary_negative: (l:list pyObj {Cons? l}) -> Tot (list pyObj)

val unary_not: (l:list pyObj {Cons? l}) -> Tot (list pyObj)

val get_iter: (l:list pyObj {Cons? l}) -> Tot (list pyObj)

val binary_multiply: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val binary_floor_divide: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val binary_modulo: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val binary_add: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val binary_subtract: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val binary_subscr: (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val build_tuple: (n:nat) -> (l:list pyObj {length l >= n}) -> Tot (list pyObj)

val build_list: (n:nat) -> (l:list pyObj {length l >= n}) -> Tot (list pyObj)

val build_map: (n:nat) -> (l:list pyObj {length l >= (op_Multiply 2 n)})
                      -> Tot (list pyObj)

val build_const_key_map: (n:nat) -> (l:list pyObj {length l >= n+1})
                                -> Tot (list pyObj)

val compare_op: nat -> (l:list pyObj {length l >= 2}) -> Tot (list pyObj)

val store_name: (i:nat) -> (l1:list string {length l1 > i})
                       -> Map.t string pyObj 
                       -> (l2:list pyObj {Cons? l2}) 
                       -> Tot (Map.t string pyObj * list pyObj)

val load_const: (i:nat) -> (l:list pyObj {length l > i}) -> list pyObj 
                       -> Tot (list pyObj)

val load_name: (i:nat) -> (l:list string {length l > i}) 
                      -> Map.t string pyObj -> Map.t string pyObj 
                      -> list pyObj -> Tot (list pyObj)

val pop_jump_if_true: nat -> nat -> (l:list pyObj {Cons? l}) -> Tot (nat * list pyObj)

val pop_jump_if_false: nat -> nat -> (l:list pyObj {Cons? l}) -> Tot (nat * list pyObj)

val jump_if_true_or_pop: nat -> nat -> (l:list pyObj {Cons? l}) -> Tot (nat * list pyObj)

val jump_if_false_or_pop: nat -> nat -> (l:list pyObj {Cons? l}) -> Tot (nat * list pyObj)

val for_iter: nat ->  nat -> (l:list pyObj {Cons? l}) -> Tot (nat * list pyObj)

val load_global: (i:nat) -> (l:list string {length l > i}) -> Map.t string pyObj 
                        -> list pyObj -> Tot (list pyObj)

val load_fast: (i:nat) -> (l:list pyObj {length l > i}) 
                      -> list pyObj -> Tot (list pyObj)

val store_fast: (i:nat) -> (l1:list pyObj {length l1 >= i}) 
                       -> (l2:list pyObj {Cons? l2}) 
                       -> Tot (list pyObj * list pyObj)

val make_function: (i:nat) -> (Map.t string pyObj) -> (l:list pyObj {if i = 0
                                                                 then length l >= 2
                                                                 else length l >= 3})
                      -> Tot (list pyObj)

val build_slice: (i:nat {i = 2 || i =3}) -> (l:list pyObj {length l >= i}) -> Tot (list pyObj)

val execBytecode: frameObj  -> All.ML (frameObj)

val runFrame: vm -> frameObj -> All.ML (vm * pyObj)
