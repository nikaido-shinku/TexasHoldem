MODULES=card deck hand authors command state main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,oUnit,str,ANSITerminal
DEPENDENCY = _tags .merlin .ocamlinit Makefile README.md checkenv.sh checkzip.sh finalcheck.sh INSTALL.txt

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

zip:
	zip texas_holdem.zip *.ml* $(DEPENDENCY)

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report texas_holdem.zip bisect*.out bisect*.coverage
