[![Build Master](https://github.com/Kakadu/zanuda/actions/workflows/master_docker.yml/badge.svg?branch=master)](https://github.com/Kakadu/zanuda/actions/workflows/master_docker.yml)
[![License](https://img.shields.io/badge/license-LGPL-blue)](https://github.com/JetBrains-Research/spla/blob/master/LICENSE.md)
[![Lints docs](https://img.shields.io/badge/Documentation-lints-yellowgreen)](https://kakadu.github.io/zanuda/lints/index.html)
[![API docs](https://img.shields.io/badge/Documentation-API-yellowgreen)](https://kakadu.github.io/zanuda/api/index.html)

[![](http://github-actions.40ants.com/Kakadu/zanuda/matrix.svg)](https://github.com/Kakadu/zanuda)

#### An OCaml linter experiment

Mostly inspired by [How possible is a clippy-like linter for OCaml?](https://discuss.ocaml.org/t/how-possible-is-a-clippy-like-linter-for-ocaml)

Lints: https://kakadu.github.io/zanuda/lints/index.html

API: https://kakadu.github.io/mylinter/api/ is currently empty

##### Usage

Examples of 'zanuda' usage could be found in the 'tests' directory. But in short:

* Compile your dune project and run this linter via

    ````
    dune build . @runtest -j3
    zanuda -dir .
    ````

    It will report found issues using OCaml's alerts

* You could read the documentation about supported lints via `zanuda -dump`. CI runs regularly uploads [information about available lints](https://kakadu.github.io/zanuda/lints/index.html) to Gihub Pages.

* You could run linter and dump the results in short JSON form. They could be processed later, for example as review comment via GitHub API. (This reporting is not implemented yet.)


##### See also

* [Ocp-lint paper](https://hal.inria.fr/hal-01352013/document)
* Lexifi's dead [dead_code_analyzer](https://github.com/LexiFi/dead_code_analyzer)
* [Camelot](https://github.com/upenn-cis1xx/camelot)
* [ometrics](https://gitlab.com/nomadic-labs/ometrics)



##### Developping

Current dependecies:

    opam install ppx_fields_conv ppx_blob base angstrom ppx_expect ppx_assert sexplib --yes

Running a single test:

    dune build && dune build @ifbool --force

Running all tests:

    dune build && dune runtest --force
