
  $ dune build @lint -p testsuite1 --force
      mylinter alias lint
  File "lib.ml", line 4, characters 0-18:
  4 | type myAst = MyAst
      ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Type name `myAst` should be in snake case
  $ dune build testlib2.cmxa
  $ dune build @lint -p testsuite2 --force
      mylinter alias lint
  File "Parsetree.mli", lines 1-4, characters 0-17:
  1 | type exprA =
  2 |   | App of exprA * exprA
  3 |   | Abs of string * exprA
  4 |   | Var of string
  Alert zanuda-linter: Type name `exprA` should be in snake case
  File "Parsetree.mli", line 2, characters 2-24:
  2 |   | App of exprA * exprA
        ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
  File "Parsetree.mli", line 3, characters 2-25:
  3 |   | Abs of string * exprA
        ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'Abs' has no documentation attribute
      mylinter alias lint
  File "lib2.ml", line 1, characters 19-37:
  1 | let nonsense1 xs = List.length xs < 0
                         ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "lib2.ml", line 3, characters 19-38:
  3 | let is_empty1 xs = List.length xs <= 0
                         ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "lib2.ml", line 7, characters 20-39:
  7 | let tautology1 xs = List.length xs >= 0
                          ^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
  File "lib2.ml", line 8, characters 19-37:
  8 | let not_empty xs = List.length xs > 0
                         ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Bad measurement of a list (with non-negative size)
