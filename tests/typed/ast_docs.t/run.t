  $ dune build @all @check
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
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
