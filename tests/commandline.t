  $ zanuda --help > /dev/null
Check untyped AST in file could be doable, but currently is almost dead.
Typed AST provide much more information
  $ touch empty.ml
  $ zanuda -I . -dump empty.ml -o /dev/null -del-prefix / -add-prefix / > /dev/null
Golint format currently is not used
  $ zanuda -ogolint 1.golint > /dev/null
  $ zanuda -dump-lints 1.json 2> /dev/null
  $ zanuda -o file.out -v 2> /dev/null
Testing config file
  $ echo  '-no-no-topelevel-eval\nasdf' > .zanuda
  $ zanuda -o file.out -v 2> /dev/null
  $ rm -fr .zanuda

  $ zanuda -dump > /dev/null
