  $ dune build
  $ zanuda -no-top_file_license -dir .
  File "StringConcat.ml", line 2, characters 1-10:
  2 |  a ^ b ^ c
       ^^^^^^^^^
  Alert zanuda-linter: Concatenating multiple strings at once (`a^b^c`) has a perfomance issue.
  
