  $ dune build
  $ dune test
$ dune describe

  $ zanuda -unused-decls . |grep -v "Base\|Angstrom\|Ppx_deriving_runtime"
  Unused declarations:
   0: Nonwrapped.nonwrapped_fac
