A small dune project is compiled in place, so the crapper sees real .cmt files.

  $ dune build

  $ ../crapper.exe .
  CRAP Report
  ===========
  Languages: ocaml
  
  Function                       Namespace                             CC    Cov%     CRAP
  ----------------------------------------------------------------------------------------
  complex                        Foo                                    8     N/A      N/A
  g                              Foo                                    4     N/A      N/A
  andor                          Foo                                    3     N/A      N/A
  guarded                        Foo                                    3     N/A      N/A
  loops                          Foo                                    3     N/A      N/A
  even                           Foo                                    2     N/A      N/A
  odd                            Foo                                    2     N/A      N/A
  add                            Foo                                    1     N/A      N/A
  local                          Foo                                    1     N/A      N/A
  mul                            Foo.Bar                                1     N/A      N/A
  twice                          Foo.Inner                              1     N/A      N/A
  bump                           Foo.counter                            1     N/A      N/A
  get                            Foo.counter                            1     N/A      N/A
  

Test code is compiled too, but must not be discovered:

  $ test -f _build/default/.foo.objs/byte/foo__Test_helper.cmt && echo exists
  exists
  $ test -f _build/default/tests/.foo_tests.objs/byte/foo_tests__Test_helper.cmt && echo exists
  exists
