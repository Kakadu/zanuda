  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -no-propose_function -dir . -ordjsonl /dev/null 2>&1 | sed -e '/^[[:space:]]*$/d' -e 's/[[:space:]]*$//'
  File "MatchBool.ml", lines 1-3, characters 20-14:
  1 | ....................match a with
  2 |   | true -> 1
  3 |   | false -> 2
  Alert zanuda-linter: Match is redundant. It's recommended to rewrite it as
                       'if a then 1 else 2'
  File "MatchBool.ml", lines 5-9, characters 20-13:
  5 | ....................match a with
  6 |   | false -> (
  7 |     Format.printf "meow %d" 42;
  8 |     1)
  9 |   | true -> 2
  Alert zanuda-linter: Match is redundant. It's recommended to rewrite it as
                       'if a then 2 else (Format.printf "meow %d" 42; 1)'
