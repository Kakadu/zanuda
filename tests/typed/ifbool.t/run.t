  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "IfBool.ml", line 2, characters 2-23:
  2 |   if true then 1 else 2
        ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if true' smells bad
  
  File "IfBool.ml", line 4, characters 14-39:
  4 | let __ f x  = if f x then true else f x
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then true' smells bad
  
  File "IfBool.ml", line 5, characters 14-40:
  5 | let __ f x  = if f x then f x else false
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Executing 'if ... then .. else false' smells bad
  
  File "IfBool.ml", line 8, characters 18-29:
  8 |   | _::xs -> foo1 (r && true) xs
                        ^^^^^^^^^^^
  Alert zanuda-linter: Conjunction with boolean smells bad
  
  File "IfBool.ml", line 9, characters 10-22:
  9 |   | _ ->  (r && false)
                ^^^^^^^^^^^^
  Alert zanuda-linter: Conjunction with boolean smells bad
  
  File "IfBool.ml", line 12, characters 5-14:
  12 | type substring =
            ^^^^^^^^^
  Alert zanuda-linter: Identifier `_record__` used somewhere else but supposed to be unused.
  File "IfBool.ml", line 13, characters 4-8:
  13 |   { name : int
           ^^^^
  Alert zanuda-linter: Identifier `_r__` used somewhere else but supposed to be unused.

