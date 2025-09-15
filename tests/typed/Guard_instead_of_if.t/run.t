  $ dune build @check
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "If_guard.ml", line 2, characters 10-31:
  2 |   | [] -> if cond then 1 else 2
                ^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Prefer guard instead of if-then-else in case construction
  File "If_guard.ml", line 6, characters 11-31:
  6 |   | [x] -> if x>0 then 1 else 2
                 ^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Prefer guard instead of if-then-else in case construction

