  $ dune build @src/check
$ dune describe
  $ echo 'ignore duniverse/' > .zanuda
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "duniverse/somelib/dune", line 4, characters 12-24:
  4 |  (libraries doesnt_exist))
                  ^^^^^^^^^^^^
  Error: Library "doesnt_exist" not found.
  -> required by library "lib1" in _build/default/duniverse/somelib
  Sexplib.Sexp.of_string: incomplete S-expression while in state Parsing_toplevel_whitespace: 

$ dune describe workspace src/
  $ ../zanuda.exe -no-top_file_license -dir src -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File '_build/default/src/Asdf.ml' doesn't have corresponding .mli interface
