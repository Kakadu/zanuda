The same code twice: [complex] is never called by the test, [complex_copy]
is fully exercised. Identical complexity, very different CRAP scores.

  $ dune build

  $ ../crapper.exe .
  CRAP Report
  ===========
  Languages: ocaml
  
  Function                       Namespace                             CC    Cov%     CRAP
  ----------------------------------------------------------------------------------------
  complex                        Cm                                     8    0.0%     72.0
  complex_copy                   Cm                                     8   90.0%      8.1
  

