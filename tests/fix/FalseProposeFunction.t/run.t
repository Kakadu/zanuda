  $ dune build
  $ zanuda -no-top_file_license -dir .
  File '_build/default/FalseProposeFunction.ml' doesn't have corresponding .mli interface
  
  File "FalseProposeFunction.ml", lines 2-5, characters 0-43:
  2 | type private_flag =
  3 |   | Private
  4 |   | Public
  5 | [@@deriving eq, show { with_path = false }]
  Alert zanuda-linter: Using `function` is recommended
  File "FalseProposeFunction.ml", lines 7-10, characters 0-43:
   7 | type closed_flag =
   8 |   | Closed
   9 |   | Open
  10 | [@@deriving eq, show { with_path = false }]
  Alert zanuda-linter: Using `function` is recommended
  File "FalseProposeFunction.ml", lines 2-5, characters 0-43:
  2 | type private_flag =
  3 |   | Private
  4 |   | Public
  5 | [@@deriving eq, show { with_path = false }]
  Error: Wrong location for replacement
  File "FalseProposeFunction.ml", line 2, characters 5-0:
  Error: Wrong location for replacement
  File "FalseProposeFunction.ml", lines 7-10, characters 0-43:
   7 | type closed_flag =
   8 |   | Closed
   9 |   | Open
  10 | [@@deriving eq, show { with_path = false }]
  Error: Wrong location for replacement
  File "FalseProposeFunction.ml", line 7, characters 5-0:
  Error: Wrong location for replacement
  $ cat fix_gen/diffs.log
  Diffs for file FalseProposeFunction.ml
  2c2
  < type private_flag =
  ---
  > function private_flag =
  7c7
  < type closed_flag =
  ---
  > function closed_flag =
  
