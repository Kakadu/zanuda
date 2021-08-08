.PHONY: all release watch test promote install

all:
	dune build

release:
	dune build --profile release

watch:
	dune build -w

test:
	dune build && dune test

promote:
	dune test --auto-promote

install:
	dune build @install
	dune install

uninstall:
	dune build @install
	dune uninstall

clean:
	dune clean
