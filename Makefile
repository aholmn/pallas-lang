make all: build run

.PHONY: build
build:
	dune build
run:
	dune exec pallas examples/fibonacci

clean:
	rm -rf _build

install: build
	cp _build/install/default/bin/pallas /usr/local/bin/pallas
