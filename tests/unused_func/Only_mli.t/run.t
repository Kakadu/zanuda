  $ dune build @check @runtest
$ dune describe
  $ dune exec ./only_mli_demo.exe #-j1 --verbose
  my_id 5 = 5
  $ ../zanuda.exe -unused-decls .
  module "Only_mli_lib" is omitted
  Unused declarations:
   0: A.incr
   1: B.my_id
# TODO: B.my_id is reported because we don't take into account external definitions
