  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir .  -ordjsonl /dev/null
  File "amb_cstrs.ml", lines 2-4, characters 2-14:
  2 | ..type 'a result = 
  3 |     | Error of string 
  4 |     | Ok of 'a..
  Alert zanuda-linter: Constructors `Error`, `Ok` of this type should not look like defaults
  File "amb_cstrs.ml", lines 6-8, characters 2-10:
  6 | ..type str_option = 
  7 |     | Some of string
  8 |     | None
  Alert zanuda-linter: Constructors `Some`, `None` of this type should not look like defaults
  File "amb_cstrs.ml", lines 10-12, characters 2-16:
  10 | ..type 'a option = 
  11 |     | Nothing
  12 |     | Some of 'a
  Alert zanuda-linter: Constructor `Some` of this type should not look like defaults
  File "amb_cstrs.ml", lines 27-29, characters 0-14:
  27 | type 'a option = 
  28 |   | Option
  29 |   | Some of 'a
  Alert zanuda-linter: Constructor `Some` of this type should not look like defaults
