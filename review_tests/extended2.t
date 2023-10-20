$ cat extended2.diff
 
  $ cat extended2.diff | ../review/parser.exe - -f MiniML/.gitignore -l 5
  Got something. It should be at 6 lines below from the first chunk header of file in diff

Currently not the all possible extension of headers are supported.
For example dissimilarity index is not. See https://git-scm.com/docs/diff-format for details
$ cat << EOF | ../review/parser.exe - -f Python/Python.opam -l 3
