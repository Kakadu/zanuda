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

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	if [ -d $(TEST_COV_D) ]; then $(RM) -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/zanuda dune runtest --no-print-directory \
          --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D)  --expect src/ --expect review/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) --expect src/ --expect review/
	@echo "Use 'xdg-open _coverage/index.html' to see coverage report"
