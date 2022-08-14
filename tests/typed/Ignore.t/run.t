  $ dune build
  $ zanuda  -no-check-filesystem -no-top_file_license -dir .
  File "Ignore.ml", line 2, characters 2-29:
  2 |   ignore (List.map ((+)1) xs);
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Unsafe ingore. It's recommended to rewrite it as 'let (_: int list) = List.map ((+) 1) xs'
  File "Ignore.ml", line 6, characters 2-41:
  6 |   Base.ignore (List.map string_of_int xs)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Unsafe ingore. It's recommended to rewrite it as 'let (_: string list) = List.map string_of_int xs'
