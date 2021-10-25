#include ../Makefile.include
# --------------------------------------------------------------------
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

all: structs exec vm

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

out:
	mkdir -p out

ocaml: out/Structs.ml out/Exec.ml out/VM.ml
	cd out; $(OCAMLOPT) Structs.ml Exec.ml VM.ml -o VM.exe

clean:
	rm -rf out *~ *.exe
