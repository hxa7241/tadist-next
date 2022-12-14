#  TADIST tool (OCaml 4.12)
#  Harrison Ainsworth / HXA7241 : 2015, 2021


EXE=tadist
SRC=hxaGeneral.mli hxaGeneral.ml utf8.mli utf8.ml tadist.mli tadist.ml tadistEpub.mli tadistEpub.ml tadistPdf.mli tadistPdf.ml tadistQuerier.mli tadistQuerier.ml tadistMelder.mli tadistMelder.ml tadistTool.mli tadistTool.ml
LIBSN=unix.cmxa str.cmxa zip.cmxa
LIBSB=unix.cma  str.cma  zip.cma
OPTS=-principal -strict-sequence -strict-formats -w +A
OPTS2=-I libs/


all: native
#all: native bytecode

native: $(EXE)
$(EXE): $(SRC)
	ocamlopt.opt -o $(EXE) -nodynlink $(OPTS) $(OPTS2) $(LIBSN) $(SRC)
	rm -f *.cm[ixo] *.o

bytecode: $(EXE)-b
$(EXE)-b: $(SRC)
	ocamlc -o $(EXE)-b -compat-32 $(OPTS) $(OPTS2) $(LIBSB) $(SRC)
	rm -f *.cm[ixo] *.o


.PHONY: clean
clean:
	rm -f *.cm[ixo] *.[ao]
	rm -f $(EXE) $(EXE)-b
