
  $ dune build @lint -p testsuite1 --force
      mylinter alias lint
  opening out file
  File "lib.ml", line 4, characters 0-18:
  4 | type myAst = MyAst
      ^^^^^^^^^^^^^^^^^^
  Alert asdf: Type name `myAst` should be in snake case
  $ dune build testlib2.cmxa
  $ dune build @lint -p testsuite2 --force
