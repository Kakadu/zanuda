$ which zanuda
  $ untyped/zanuda.exe --help > /dev/null
Check untyped AST in file could be doable, but currently is almost dead.
Typed AST provide much more information
  $ touch empty.ml
  $ untyped/zanuda.exe -I . -dump empty.ml -o /dev/null -del-prefix / -add-prefix / > /dev/null
Golint format currently is not used
  $ untyped/zanuda.exe -ogolint 1.golint > /dev/null
  $ untyped/zanuda.exe -dump-lints 1.json 2> /dev/null
  $ untyped/zanuda.exe -o file.out -v 2> /dev/null
Testing config file
  $ echo  '-no-no-topelevel-eval\nasdf' > .zanuda
  $ untyped/zanuda.exe -o file.out -v 2> /dev/null
  $ rm -fr .zanuda

  $ untyped/zanuda.exe -dump > /dev/null
