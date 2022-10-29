#include ../Makefile.include
# --------------------------------------------------------------------
# FSTAR_HOME = /home/akarkour/.fstar

ifdef FSTAR_HOME
   FSTAR_ULIB=$(FSTAR_HOME)/ulib
else
   # FSTAR_HOME not defined, assume fstar.exe installed through opam
   # or binary package, and reachable from PATH
   FSTAR_ULIB=$(dir $(shell which fstar.exe))/../lib/fstar
endif

include $(FSTAR_ULIB)/gmake/z3.mk
include $(FSTAR_ULIB)/gmake/fstar.mk

ifeq ($(OS),Windows_NT)
  ifndef FSTAR_HOME
     $(error "Please define the `FSTAR_HOME` variable before including this makefile.")
  endif
  MSBUILD = $(FSTAR_HOME)/src/msbuild.bat
else
  # If can't find msbuild, use xbuild, but throw a warning
  MSBUILD = $(shell which msbuild || (echo '\n\n\033[0;31mWarning:\033[0m could not find "msbuild", trying (deprecated) "xbuild"\n\n'>&2; which xbuild))
endif

# we ignore the return result in benchmark runs because we can have micro-benchmarks which
# produce error asserts when executed with '--admit_smt_queries true'
%.uver: %.fst
	$(Q)$(BENCHMARK_PRE) $(FSTAR)  $^

%.fail-uver: %.fst
	(! $(FSTAR) $^ >/dev/null 2>&1) || (echo "NEGATIVE TEST FAILED ($@)!" ; false)

######################################################## Makefile.include

include $(FSTAR_ULIB)/ml/Makefile.include

all: structs utils pyexception pybool pylist pydict pyfunction pyint pynone pystring pytuple pyslice pybuiltinobjects exec vm ocaml

vm: out VM.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'VM' --odir out --codegen OCaml VM.fst #--record_hints
	@#$(OCAMLOPT) out/VM.ml -o VM.exe
	@#./VM.exe

structs: out Structs.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'Structs' --odir out --codegen OCaml Structs.fst #--record_hints
	@#$(OCAMLOPT) out/TestSeq.ml -o testseq.exe
	@#./testseq.exe

exec: out Exec.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'Exec' --odir out --codegen OCaml Exec.fst #--record_hints

utils: out Utils.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'Utils' --odir out --codegen OCaml Utils.fst #--record_hints

pybool: out PyBool.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyBool' --odir out --codegen OCaml PyBool.fst #--record_hints

pydict: out PyDict.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyDict' --odir out --codegen OCaml PyDict.fst #--record_hints

pyfunction: out PyFunction.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyFunction' --odir out --codegen OCaml PyFunction.fst #--record_hints

pyint: out PyInt.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyInt' --odir out --codegen OCaml PyInt.fst #--record_hints

pylist: out PyList.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyList' --odir out --codegen OCaml PyList.fst #--record_hints

pynone: out PyNone.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyNone' --odir out --codegen OCaml PyNone.fst #--record_hints

pystring: out PyString.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyString' --odir out --codegen OCaml PyString.fst #--record_hints

pytuple: out PyTuple.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyTuple' --odir out --codegen OCaml PyTuple.fst #--record_hints

pyexception: out PyException.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyException' --odir out --codegen OCaml PyException.fst #--record_hints

pyslice: out PySlice.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PySlice' --odir out --codegen OCaml PySlice.fst #--record_hints

pybuiltinobjects: out PyBuiltinObjects.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'PyBuiltinObjects' --odir out --codegen OCaml PyBuiltinObjects.fst #--record_hints

test: out test/Test.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract 'Test' --odir out --codegen OCaml test/Test.fst #--record_hints

autotestTranslate: out test/$(NAME).fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --extract '$(NAME)' --odir out --codegen OCaml test/$(NAME).fst #--record_hints

out:
	mkdir -p out

ocaml: out/Structs.ml out/Exec.ml out/VM.ml
	cd out; $(OCAMLOPT) Structs.ml Utils.ml PyException.ml PyBool.ml PyList.ml PyDict.ml PyFunction.ml PyInt.ml PyNone.ml PyString.ml PyTuple.ml PySlice.ml PyBuiltinObjects.ml Exec.ml VM.ml

autotestCompile:
	cd out; $(OCAMLOPT) Structs.ml Utils.ml PyException.ml PyBool.ml PyList.ml PyDict.ml PyFunction.ml PyInt.ml PyNone.ml PyString.ml PyTuple.ml PySlice.ml PyBuiltinObjects.ml Exec.ml VM.ml $(NAME).ml -o $(NAME).exe

clean:
	rm -rf out *~ *.exe
