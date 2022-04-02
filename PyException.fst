module PyException

open Structs
open Utils

let createException s =
  let obj: cls = {
    name= "Exception";
    pid = 0;
    value = EXCEPTION s;
    fields = emptyMap;
    methods = emptyMap
  } in obj
  
