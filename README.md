[![Build Master](https://github.com/Kakadu/mylinter/actions/workflows/master.yml/badge.svg?branch=master)](https://github.com/Kakadu/mylinter/actions/workflows/master.yml)

[![](http://github-actions.40ants.com/Kakadu/mylinter/matrix.svg)](https://github.com/Kakadu/mylinter)

#### An OCaml linter experiment

Mostly inspired by [How possible is a clippy-like linter for OCaml?](https://discuss.ocaml.org/t/how-possible-is-a-clippy-like-linter-for-ocaml)

Lints: https://kakadu.github.io/zanuda

API: https://kakadu.github.io/mylinter/api/ is currently empty

##### See also

* [Ocp-lint paper](https://hal.inria.fr/hal-01352013/document)
* Lexifi's dead [dead_code_analyzer](https://github.com/LexiFi/dead_code_analyzer)
* [Camelot](https://github.com/upenn-cis1xx/camelot)
* [ometrics](https://gitlab.com/nomadic-labs/ometrics)



##### Developing

Running a single test:

    dune build && dune build @ifbool --force

Running all tests:

    dune build && dune runtest --force
