  $ dune build
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "Parsetree.mli", line 2, characters 2-16:
  2 |   | App of t * t
        ^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
