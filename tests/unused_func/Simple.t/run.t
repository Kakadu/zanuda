  $ dune build @check
$ dune describe
  $ zanuda -unused-decls .
  module "Custom_lib" is omitted
  unused decl: Simple.Meow.meow
  unused decl: Simple.Meow.woof
  unused decl: Custom_lib_impl.incr
# Custom_lib_impl.incr is not found because a custom public name for library 'custom_lib'
