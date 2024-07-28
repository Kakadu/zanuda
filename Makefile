.PHONY: all release watch test promote install deps

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
	dune build @install -p reviewer,zanuda
	dune install reviewer zanuda

uninstall:
	dune build @install
	dune uninstall

clean:
	@dune clean
	@$(RM) -r _coverage

deps:
	opam install --confirm-level=yes \
		ppx_blob curly dune ppx_expect stdune angstrom sexplib \
		ppx_fields_conv ppx_show bisect_ppx

ODIG_SWITCHES = --odoc-theme=odig.gruvbox.light
ODIG_SWITCHES += --no-tag-index
ODIG_SWITCHES += --no-pkg-deps
.PHONY: odig
odig:
	odig odoc $(ODIG_SWITCHES) zanuda reviewer
	@echo 'To look at the doc run: xdg-open $$(odig cache path)/html/index.html'

TEST_COV_D = /tmp/zanudacov
COVERAGE_OPTS = --coverage-path $(TEST_COV_D) --expect src/ --expect review/ --expect review_tests/

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
