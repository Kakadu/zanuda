  $ dune build
$ find .
$ echo $PATH
  $ mylinter -dir .
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
  
