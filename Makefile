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
COVERAGE_OPTS = --coverage-path $(TEST_COV_D) --expect src/ --expect review/

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	$(RM) -r $(TEST_COV_D)
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/zanuda dune runtest --no-print-directory \
          --instrument-with bisect_ppx --force
	bisect-ppx-report html $(COVERAGE_OPTS)
	bisect-ppx-report summary $(COVERAGE_OPTS)
	@echo "Use 'xdg-open _coverage/index.html' to see coverage report"
