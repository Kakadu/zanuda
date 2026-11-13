A small dune project is compiled in place, so the crapper sees real .cmt files.

  $ dune build

  $ ../crapper.exe .
  CRAP Report
  ===========
  Languages: ocaml
  
  Function                       Namespace                             CC    Cov%     CRAP
  ----------------------------------------------------------------------------------------
  complex                        Foo                                    8    0.0%     72.0
  g                              Foo                                    4    0.0%     20.0
  andor                          Foo                                    3    0.0%     12.0
  guarded                        Foo                                    3    0.0%     12.0
  loops                          Foo                                    3  100.0%      3.0
  odd                            Foo                                    2   66.7%      2.1
  even                           Foo                                    2  100.0%      2.0
  local                          Foo                                    1    0.0%      2.0
  mul                            Foo.Bar                                1    0.0%      2.0
  add                            Foo                                    1  100.0%      1.0
  twice                          Foo.Inner                              1  100.0%      1.0
  bump                           Foo.counter                            1  100.0%      1.0
  get                            Foo.counter                            1  100.0%      1.0
  


  $ ../crapper.exe --use-existing-coverage --threshold 71 . > /dev/null
  [2]
  $ ../crapper.exe --use-existing-coverage --threshold 72 . > /dev/null

  $ ../crapper.exe --no-coverage . | grep complex
  complex                        Foo                                    8     N/A      N/A
