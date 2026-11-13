A small dune project is compiled in place, so the crapper sees real .cmt files.

  $ dune build
  $ ../crapper.exe .
  foo.ml:1-1  Foo.add
  foo.ml:2-2  Foo.even
  foo.ml:3-3  Foo.odd
  foo.ml:9-9  Foo.local
  foo.ml:12-12  Foo.Inner.twice
  foo.ml:17-17  Foo.counter.get
  foo.ml:18-18  Foo.counter.bump
  bar.ml:1-1  Foo.Bar.mul

Test code is compiled too, but must not be discovered:

  $ test -f _build/default/.foo.objs/byte/foo__Test_helper.cmt && echo exists
  exists
  $ test -f _build/default/tests/.foo_tests.objs/byte/foo_tests__Test_helper.cmt && echo exists
  exists
