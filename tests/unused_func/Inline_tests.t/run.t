  $ dune build
  $ dune test
$ dune describe

  $ zanuda -unused-decls . |grep -v "Base\|Angstrom\|Ppx_deriving_runtime"
  unused decl: Nonwrapped.nonwrapped_fac
