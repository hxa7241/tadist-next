#  TADIST tool (OCaml 4.10)
#  Harrison Ainsworth / HXA7241 : 2015, 2020


EXE=tadist
SRC=hxaGeneral.mli hxaGeneral.ml tadist.mli tadist.ml tadistEpub.mli tadistEpub.ml tadistPdf.mli tadistPdf.ml tadistQuerier.mli tadistQuerier.ml tadistMelder.mli tadistMelder.ml tadistTool.mli tadistTool.ml
LIBSN=unix.cmxa str.cmxa utf8filter.cmxa camlpdf.cmxa zip.cmxa
LIBSB=unix.cma  str.cma  utf8filter.cma  camlpdf.cma  zip.cma
OPTS=-principal -strict-sequence -strict-formats -w +A
OPTS2=-I libs/


all: exes

exes: $(EXE) $(EXE)b
$(EXE): $(SRC)
	ocamlopt.opt -o $(EXE) -nodynlink $(OPTS) $(OPTS2) $(LIBSN) $(SRC)
	rm -f *.cm[ixo] *.o
$(EXE)b: $(SRC)
	ocamlc -o $(EXE)b -compat-32 $(OPTS) $(OPTS2) $(LIBSB) $(SRC)
	rm -f *.cm[ixo] *.o


.PHONY: clean
clean:
	rm -f *.cm[ixo] *.[ao]
	rm -f $(EXE) $(EXE)b
