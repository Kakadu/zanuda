all:
	dune build

release:
	dune build -p release

watch:
	dune build -w
