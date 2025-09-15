  $ dune build 2>&1 #| sed 's/test_Format.ml\[[[:digit:]]\+,[[:digit:]]\++[[:digit:]]\+\]//g'
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "test_Format.ml", line 1, characters 25-35:
  1 | let _1 x = Format.printf "a\"%s\"b" x 
                               ^^^^^^^^^^
  Alert zanuda-linter: The format string is too much verbose (rewrite "%s" -> %S)
