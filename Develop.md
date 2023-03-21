#### Hacking

##### Main

The tools accpts a directory where [dune](https://dune.rtfd.io)-based project is build

    linter -dir <DIR>

It parses the output of `dune describe` to get information about the files in the project. After that it applies lints in that files, one by one. Everything is tested with OCaml 4.14.1

##### Source tree mapping

Main executable:
  * `src/main.ml`
  * `src/Config.ml` -- handling command line parameters
  * `src/LoadDune.ml[i]` -- parsing dune project configuration
  * `src/utils.ml` -- various functions to output found lints as JSON
  * `LINT.mli` -- module type which every lint should implement
  * `src/CollectedLints.ml` -- storing lints found in the file

Library implementing first class pattern matching on OCaml 4.14.1's typedtree
  * `src/pattern/`

Typed lints implemented. They analyze OCaml's TypedTree
  * `src/typed/`

Untyped lints analyze OCaml's ParseTree (highly likely be a legacy).
  * `src/untyped/`
