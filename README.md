# Typing and Semantics Formalization of Python's Execution Machinery

  

## Dependencies:

- Install F* and OCaml from [here](https://github.com/FStarLang/FStar/blob/master/INSTALL.md) (using opam)

- Python 3.8 or 3.9

## Directory Structure

- README
- **src**
	- Structs.fst  --- Conatins types and structures
	- Exec.fst	  --- Contains the main execution loop 
	- Exec.fsti	  --- Contains typing rules and formal verification 
	- **out**		  --- Contains compiled and executable OCaml files of Py*
	- MAKEFILE
	-  ....
	
- **test**
	- autotest.py	--- Handles autmatic running of the code
	- config.py 		--- configuration file to set testing flags
	- translator.py		--- A class that translates cpython's codeobject to py* codeobject
	- test.py			--- Use this class to define your own test cases 
	- **hand_crafted** --- Contains correctness test cases
	- **performance** --- Conatains performance test cases
  

## To Re-Compile
**The code is already compiled, so you can directly use it.**  

If you are facing any issues and want to recompile:
	- Go to src ($ cd src)
	- Write make ($ make)

## To Run Test Cases:

- Go to test ($ cd test)
- Use autotest.py to run the tests. E.g., **($ python3 autotest.py hand_crafted/variables.py Test)**
	-  **hand_crafted/variables.py**: Test file.
	-  **Test**: Name of the generated f* file
