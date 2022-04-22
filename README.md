# Py*: Formalization of Python's Verifiable Bytecode and Virtual Machine in F* 

## Project Descritopn
When writing a program, it is very important that the written code matches its intended behavior/specifications. Similar to all other programs, implementations of Programming Languages also need to match their own specifications, because it is hard to trust any program that is written in a language that does not do that. To address this problem, Computer Scientists formalize programming languages using Formal Semantics rules that describe how each expression in the language should behave. These rules help in guiding the implementation process and eliminate ambiguities. When it comes to Python, Even though Python has become one of the most important programming languages, it still lacks a formal implementation as well as several formal verification methods. The reason for that is that Python was not designed with formal rigor, which made it lack full formal semantics that is needed to formalize and describe the behavior of its complex functionalities. Since formalizing Python Source code is hard because of its many complex functionalities and inconsistencies, a more realistic and attainable goal is to formalize Python’s Bytecode. This way we focus on a smaller set of instructions that have simpler functionalities, which makes it easier to formalize and formally verify code written in it. Moreover, in the end, Bytecode is what is being executed by the interpreter. In this project, we define formal semantics rules for Python's Bytecode, and embed them in the theorem prover F*. Following that we extract efficient executable OCaml code of our embedding, which could be used to interpret Python Bytecode and to find bugs in other interpreters.

## Dependencies:
- Install F* from [here](https://www.fstar-lang.org/#download)
- Install OCaml from [here](https://ocaml.org/)

## Setting up the Makefile
- Change teh variable FSTAR_HOME in the Makefile to path of fstar in your local machine

## To Run Test Cases:
- Write your testcase in the file **Test.fst**
- Run ($ make all), which translates your code from fstar to ocaml
- In the file **Test.ml** add this line at the end: **let (p : unit) = print_string(Utils.print_pyObj res)**
- Run ($ make ocaml), which compiles the ocaml files
- Run ($ ./out/Test.exe), which executes the test case in the test file and prints the result
