build:
	ocamlopt -c token.ml
	ocamlopt -c lexer.ml
	ocamlopt -c parser.ml
	ocamlopt -c interpreter.ml
	ocamlopt -o interpreter str.cmxa lexer.cmx token.cmx parser.cmx interpreter.cmx

byte:
	ocamlc -c token.ml
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c interpreter.ml
	ocamlc -o interpreter str.cma lexer.cmo token.cmo parser.cmo interpreter.cmo

run:
	./interpreter

clean:
	rm -f *.cmx
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.o
	rm -f interpreter

