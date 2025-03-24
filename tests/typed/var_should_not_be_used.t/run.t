  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-eta_reduction -no-top_file_license -dir .  -ordjsonl /dev/null
  File "lib.ml", line 1, characters 8-12:
  1 | let rec _foo x = _foo x
              ^^^^
  Alert zanuda-linter: Identifier `_foo` used somewhere else but supposed to be unused.
  File "lib.ml", line 2, characters 4-8:
  2 | let _boo x = 1+x
          ^^^^
  Alert zanuda-linter: Identifier `_boo` used somewhere else but supposed to be unused.
  File "lib.ml", line 6, characters 6-11:
  6 |   let _true = "true"  in
            ^^^^^
  Alert zanuda-linter: Identifier `_true` used somewhere else but supposed to be unused.
  File "lib.ml", line 10, characters 6-11:
  10 |   let _true = "true"  in
             ^^^^^
  Alert zanuda-linter: Identifier `_true` used somewhere else but supposed to be unused.
  File "lib.ml", line 20, characters 12-14:
  20 | let _fcases _x = function true -> 1 | false ->  2+_x
                   ^^
  Alert zanuda-linter: Identifier `_x` used somewhere else but supposed to be unused.
  File "lib.ml", line 21, characters 11-13:
  21 | let _fbody _x = 2+_x
                  ^^
  Alert zanuda-linter: Identifier `_x` used somewhere else but supposed to be unused.
