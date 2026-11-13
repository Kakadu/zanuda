A small dune project is compiled in place, so the crapper sees real .cmt files.

  $ dune build
  $ ../crapper.exe .
  _build/default/.foo.objs/byte/foo.cmt
  _build/default/.foo.objs/byte/foo__Bar.cmt

Test code is compiled too, but must not be discovered:

  $ test -f _build/default/.foo.objs/byte/foo__Test_helper.cmt && echo exists
  exists
  $ test -f _build/default/tests/.foo_tests.objs/byte/foo_tests__Test_helper.cmt && echo exists
  exists
