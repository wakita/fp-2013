.SUFFIXES: .mli .ml .mli .cmi .cmo

TRASH = ../*~ ../*/*~ $(RESULT)

all: otemae

clean:
	rm -f *.cmi *.cmo otemae lexer.ml parser.mli parser.ml

PHONY: .parser
.parser: parser.mly
	ocamlyacc parser.mly
parser.ml: .parser
parser.mli: .parser

lexer.ml: lexer.mll
	ocamllex $^

.mli.cmi:
	ocamlc -c $<
.ml.cmo:
	ocamlc -c $<

ast.cmo: ast.cmi
parser.cmo: parser.cmi

otemae: ast.cmo parser.cmo lexer.cmo otemae.cmo
	ocamlc -o $@ $^

install: otemae
	cp $^ ../bin
