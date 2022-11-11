module PyBuiltinFunctions

open Structs
open Utils

(* 
 - Create a map that maps builtin function names to their functionobj 
   implementation. This requires us to write functions in python and
   then get their codeobj representation.

 - Another sugggestion is: instead of finding bytecode representation
   of the builtin functions (which is gonna be hard), we add a field
   to functionobj that indicates if a function is builtin or not, and
   if it is then we use F* implementation (not bytecode implementation)
*)
