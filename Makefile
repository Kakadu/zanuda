.PHONY: all release watch test promote install

all:
	dune build

release:
	dune build --profile release

watch:
	dune test -w

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
	@dune clean
	@$(RM) -r _coverage

TEST_COV_D = /tmp/zanudacov

.PHONY: test_coverage
test_coverage:
	if [ -d $(TEST_COV_D) ]; then rm -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/zanuda dune runtest --no-print-directory \
          --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D) --expect src/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) --expect src/
