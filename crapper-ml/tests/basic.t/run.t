A small dune project is compiled in place, so the crapper sees real .cmt files.

  $ dune build
  $ ../crapper.exe .
  foo.ml:1-1  Foo.add  complexity=1
  foo.ml:2-2  Foo.even  complexity=2
  foo.ml:3-3  Foo.odd  complexity=2
  foo.ml:9-9  Foo.local  complexity=1
  foo.ml:12-12  Foo.Inner.twice  complexity=1
  foo.ml:17-17  Foo.counter.get  complexity=1
  foo.ml:18-18  Foo.counter.bump  complexity=1
  foo.ml:21-28  Foo.complex  complexity=8
  foo.ml:30-33  Foo.guarded  complexity=3
  foo.ml:35-38  Foo.loops  complexity=3
  foo.ml:40-40  Foo.andor  complexity=3
  foo.ml:42-45  Foo.g  complexity=4
  bar.ml:1-1  Foo.Bar.mul  complexity=1

Test code is compiled too, but must not be discovered:

  $ test -f _build/default/.foo.objs/byte/foo__Test_helper.cmt && echo exists
  exists
  $ test -f _build/default/tests/.foo_tests.objs/byte/foo_tests__Test_helper.cmt && echo exists
  exists
