.PHONY: all release watch test promote install

all:
	dune build

release:
	dune build -p release

watch:
	dune build -w

test:
	dune build && dune build @runtest

promote:
	dune build @runtest --auto-promote

install:
	dune build @install
	dune install

clean:
	dune clean
