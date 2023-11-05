$ cat extended2.diff
 
  $ cat extended2.diff | ../review/parser.exe - -f MiniML/.ocamlformat -l 5 #-vdp #-vlp
  Can't find 'MiniML/.ocamlformat' line 5 in the diff

Currently not the all possible extension of headers are supported.
For example dissimilarity index is not. See https://git-scm.com/docs/diff-format for details
$ cat << EOF | ../review/parser.exe - -f Python/Python.opam -l 3
