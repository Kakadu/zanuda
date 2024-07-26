#### Hacking

##### Main

The tool accepts a directory where [dune](https://dune.rtfd.io)-based project is located and was built

    zanuda -dir <DIR>

It parses the output of `dune describe` to get information about the files in the project. After that it applies lints on these files, one by one. Everything is tested with OCaml 4.14.1, and kind of specialized for this version of compiler, because we implemented Typedtree-specific first class patterns.

##### Source tree mapping

Main executable:
  * `src/main.ml`
  * `src/Config.ml` -- handling command line parameters
  * `src/Load_dune.ml[i]` -- parsing dune project configuration
  * `src/utils.ml` -- various functions to output found lints as JSON
  * `LINT.mli` -- module type which every lint should implement
  * `src/Collected_lints.ml` -- storing lints found in the file

A library implementing first class pattern matching on OCaml 4.14.x's typedtree
  * `src/pattern/`

Typed lints that has been implemented. They analyze OCaml's TypedTree
  * `src/typed/`

Untyped lints analyze OCaml's ParseTree. They are needed in rare cases, for example, compiler desugars `@@` before construction of typedtree.
  * `src/untyped/`
