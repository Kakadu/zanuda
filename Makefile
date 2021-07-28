.PHONY: all release watch test

all:
	dune build

release:
	dune build -p release

watch:
	dune build -w

test:
	dune build @runtest

install:
	dune build @install
	dune install
