
  $ dune build @lint -p testsuite1 --force
$ dune build testlib2.cmxa
  $ dune build @lint -p testsuite2 --force
  $ dune build @lint -p testsuite3 --force
  $ dune build @lint -p testsuite4 --force
  (cd _build/default && /home/kakadu/asp/zanuda/_build/install/default/bin/zanuda startingWildcard.ml -ordjsonl /tmp/out.rdjsonl -ws .)
  File "startingWildcard.ml", line 1, characters 8-12:
  1 | let rec _foo x = _foo x
              ^^^^
  Alert zanuda-linter: Identifier `_foo` used somewhere else but supposed to be unused.
  File "startingWildcard.ml", line 2, characters 4-8:
  2 | let _boo x = 1+x
          ^^^^
  Alert zanuda-linter: Identifier `_boo` used somewhere else but supposed to be unused.
  File "startingWildcard.ml", line 6, characters 6-11:
  6 |   let _true = "true"  in
            ^^^^^
  Alert zanuda-linter: Identifier `_true` used somewhere else but supposed to be unused.
$ echo $PATH
$ which zanuda
$ dune build @lint -p testsuite4 --force
$ cat _build/default/parser1.ml
