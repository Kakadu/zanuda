Load_dune.analyze_dir in several situations: a quiet run, a verbose run
and a run with a missing .cmt file.

  $ dune build

A quiet run on a fixture without unused declarations prints nothing:

  $ ../zanuda.exe -unused-decls .

Verbose mode reports the cmt files it reads and the declarations it
collected; the generated wrapper module of the wrapped library is
omitted and never read:

  $ ../zanuda.exe -v -unused-decls .
  Reading cmt[i] file '_build/default/.demo.eobjs/byte/dune__exe__Demo.cmt'
  Reading cmt[i] file '_build/default/.loadfix.objs/byte/loadfix__Other.cmt'
  used:
   0: Loadfix.Other.hidden

A .cmt file missing from the build tree:

  $ rm -f _build/default/.loadfix.objs/byte/loadfix__Other.cmt
  $ ../zanuda.exe -unused-decls .
  File '_build/default/.loadfix.objs/byte/loadfix__Other.cmt' doesn't exist. Maybe some of source files are not compiled?
