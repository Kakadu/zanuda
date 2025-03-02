  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -no-propose_function -dir .  -ordjsonl /dev/null
  File "MatchBool.ml", lines 1-3, characters 20-14:
  1 | ....................match a with 
  2 |   | true -> 1
  3 |   | false -> 2
  Alert zanuda-linter: Match is redundant. It's recommended to rewrite it as 'if a then 1 else 2'
  File "MatchBool.ml", lines 6-10, characters 20-13:
   6 | ....................match a with
   7 |   | false -> (
   8 |     Format.printf "meow";
   9 |     1)
  10 |   | true -> 2
  Alert zanuda-linter: Match is redundant. It's recommended to rewrite it as 'if a
  then 2
  else
    (Format.printf
       (CamlinternalFormatBasics.Format
          ((CamlinternalFormatBasics.String_literal
              ("meow", CamlinternalFormatBasics.End_of_format)), "meow"));
     1)'
