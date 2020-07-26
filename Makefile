build:
	ocamlopt -c token.ml
	ocamlopt -c lexer.ml
	ocamlopt -c parser.ml
	ocamlopt -c parser.cmx env.ml
	ocamlopt -c env.cmx interpreter.ml
	ocamlopt -o interpreter env.cmx str.cmxa lexer.cmx token.cmx parser.cmx interpreter.cmx

run:
	./interpreter examples/variable.psc

clean:
	rm -f *.cmx
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.o
	rm -f interpreter
