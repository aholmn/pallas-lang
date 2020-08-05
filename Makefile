make all: build run

build:
	ocamlopt -c token.ml
	ocamlopt -c lexer.ml
	ocamlopt -c ast.ml
	ocamlopt -c parser.ml
	ocamlopt -c interpreter.ml
	ocamlopt -o interpreter ast.cmx str.cmxa lexer.cmx token.cmx parser.cmx interpreter.cmx
run:
	./interpreter examples/variable.psc

clean:
	rm -f *.cmx
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.o
	rm -f interpreter
