  $ cat no_new_line.diff | ../review/parser.exe - -f MiniML/.ocamlformat -l 5
  No file "MiniML/.ocamlformat" in the list of parsed chunks
  Can't find 'MiniML/.ocamlformat' line 5 in the diff
  $ ../review/parser.exe -diff no_new_line.diff -f MiniML/.ocamlformat -l 5
  No file "MiniML/.ocamlformat" in the list of parsed chunks
  Can't find 'MiniML/.ocamlformat' line 5 in the diff
  $ ../review/parser.exe -vdp -vlp -help
  A diff parse tool designed to get a line number in the DIFF file with changes about specified source file and it's line
    -  Use stdin (default)
    -diff [file] Use this .diff file
    -f [file] Lookup for the file
    -l [NUMBER] Lookup for line in the file
    -vdp  Enable logging in the diff parser
    -vlp  Enable logging in the line parser
    -help  Display this list of options
    --help  Display this list of options
  $ ../review/parser.exe -
  File or line was not initialized
  [1]
  $ echo 'asdf' | ../review/parser.exe -
  Parsing failed: : end_of_input
  [1]
  $ ../review/parser.exe - anonymous
  Anonymous arguments ('anonymous') are not supported
  [1]
