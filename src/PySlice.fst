module PySlice

open Structs
open Utils

let createSlice (start: option int) (stop: option int) (step: option int) =
  let obj: cls = {
    name = "slice";
    pid = 0;
    value = SLICE start stop step;
    fields = emptyMap;
    methods = emptyMap
  } in obj
