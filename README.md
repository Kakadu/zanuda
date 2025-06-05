[![Build Master](https://github.com/Kakadu/zanuda/actions/workflows/master_docker.yml/badge.svg?branch=master)](https://github.com/Kakadu/zanuda/actions/workflows/master_docker.yml)
[![License](https://img.shields.io/badge/license-LGPL-blue)](https://github.com/JetBrains-Research/spla/blob/master/LICENSE.md)
[![Lints docs](https://img.shields.io/badge/Implemented-lints-yellowgreen)](https://kakadu.github.io/zanuda/lints/index.html)
[![API docs](https://img.shields.io/badge/Documentation-API-yellowgreen)](https://kakadu.github.io/zanuda/api/index.html)
[![Coverage Status](https://coveralls.io/repos/github/Kakadu/zanuda/badge.svg?branch=master)](https://coveralls.io/github/Kakadu/zanuda?branch=master)
[![opam package status](https://badgen.net/opam/v/zanuda)](https://opam.ocaml.org/packages/zanuda)

[![](http://github-actions.40ants.com/Kakadu/zanuda/matrix.svg)](https://github.com/Kakadu/zanuda)

## A linter for OCaml+dune projects

A *linter* is a tool that discovers bugs and obscurities in the code.
The name was invented in 1978 paper of S. C. Johnson: [Lint, a C Program Checker](http://squoze.net/UNIX/v7/files/doc/15_lint.pdf).

[This is a list of currently supported analyses.](https://kakadu.github.io/zanuda/lints/index.html)

From techinical point of view, most of the lints study Typedtree, so the **input** for linter should be a **fully compiled dune project**.
There is a support of linters that study untyped tree, but I don't expect them to be widely used.
These two abstract trees don't allow to find all possible code quirks.
Currently, we delegate job of detecting something like `f(1)(2)(3)` to [ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

This linter is inspired by:
* [How possible is a clippy-like linter for OCaml?](https://discuss.ocaml.org/t/how-possible-is-a-clippy-like-linter-for-ocaml)
* My experience in teaching OCaml at my local university.

But I'm aware that there are other linters like [Camelot](https://github.com/upenn-cis1xx/camelot) for OCaml and [Clippy](https://github.com/rust-lang/rust-clippy) Rust.

##### Usage

Examples of 'zanuda' usage could be found in the 'tests' directory. But in short:

* Compile your dune project and run this linter via

    ````
    dune build @check @runtest -j3
    zanuda -dir .
    ````

    It will report found issues using OCaml's alerts

* You could read the documentation about supported lints via `zanuda -dump`. CI runs regularly uploads [information about available lints](https://kakadu.github.io/zanuda/lints/index.html) to GitHub Pages.

* You could run linter and dump the results in short JSON form: `zanuda -dir . -ordjsonl /tmp/1.rdjsonl`.
  They could be processed later, for example as review comment via GitHub API.
  (This reporting is not implemented yet.)


##### Developping

To get dependecies you could use standard dune/opam machinery:

    opam pin add ./ -n
    opam install . --deps-only --with-test --with-doc

Running a single test:

    dune b @tests/typed/License

Running all tests:

    dune test


##### See also

* [Rescript Reanalyze](https://github.com/rescript-association/reanalyze)
* [Sem grep](https://github.com/semgrep/semgrep)
* [Ocp-lint paper](https://hal.inria.fr/hal-01352013/document)
* Lexifi's dead [dead_code_analyzer](https://github.com/LexiFi/dead_code_analyzer)
* [Camelot](https://github.com/upenn-cis1xx/camelot)
* [ometrics](https://gitlab.com/nomadic-labs/ometrics)
