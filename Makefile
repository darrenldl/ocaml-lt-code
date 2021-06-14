SRCFILES = src/*.ml src/*.mli

OCAMLFORMAT = ocamlformat \
	--inplace \
	$(SRCFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: lib
lib :
	dune build src

.PHONY: test
test : lib
	OCAMLRUNPARAM=b dune runtest --force src/

.PHONY: cov-desc-test
cov-desc-test : desc
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force desc/
	bisect-ppx-report html

.PHONY: cov-test
cov-test : lib
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force tests/
	bisect-ppx-report html

.PHONY: debug
debug : lib
	dune exec ./debug/main.exe

.PHONY: debug-parse
debug-parse : lib
	dune exec ./debug-parse/main.exe

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)

.PHONY : clean
clean:
	dune clean
