
  $ dune build @lint -p testsuite1 --force
      mylinter alias lint
  File "lib.ml", line 4, characters 0-18:
  4 | type myAst = MyAst
      ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Type name `myAst` should be in snake case
  File "lib.ml", line 7, characters 0-3:
  7 | 1+1;;
      ^^^
  Alert zanuda-linter: Toplevel eval not recommended
$ dune build testlib2.cmxa
  $ dune build @lint -p testsuite2 --force
      mylinter alias lint
  File "ast.mli", lines 1-4, characters 0-17:
  1 | type exprA =
  2 |   | App of exprA * exprA
  3 |   | Abs of string * exprA
  4 |   | Var of string
  Alert zanuda-linter: Type name `exprA` should be in snake case
  File "ast.mli", line 2, characters 2-24:
  2 |   | App of exprA * exprA
        ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
  File "ast.mli", line 3, characters 2-25:
  3 |   | Abs of string * exprA
        ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'Abs' has no documentation attribute
  $ dune build @lint -p testsuite3 --force
# no screaming yet but should be
