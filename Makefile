default: parsec

parsec : src/parsec.native

test : src/test.native

%.native: 
	ocamlbuild $@ -cflags -annot -use-ocamlfind

.PHONY: default

.PHONY: clean
	clean:
	echo Removing _build
	rm -r _build
