
$ diff -N -u old new

  $ cat blob1.diff | ./parser.exe - -f new/changed.txt -l 2
  new/changed.txt
  Got something. It should be at pos 147 on line 5
$ diff -N -u old/changed.txt new/changed.txt | ./parser.exe - -f new/changed.txt -l 2
new/changed.txt
got something
