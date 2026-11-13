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
  

With --no-match-cases the match cases are not counted; the same code
scores lower everywhere and the metric can no longer tell a two-path
[try] from straight-line code:

  $ ../crapper.exe --use-existing-coverage --no-match-cases . | grep complex
  complex                        Cm                                     5    0.0%     30.0
  complex_copy                   Cm                                     5   90.0%      5.0

