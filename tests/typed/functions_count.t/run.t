  $ dune build
  $ mylinter -dir . -count-fun
  There are no more than 9 functions in _build/default/Functions.ml
  $ mylinter -dir . -count-fun -v
  Trying lint 'no_docs_parsetree' on file '_build/default/Functions.ml'
  Reading cmt[i] file '_build/default/.test_functions_counting.objs/byte/functions.cmt'
  Analyzing cmt: _build/default/Functions.ml
  Function found in Functions.ml:1
  Function found in Functions.ml:3
  Function found in Functions.ml:5
  Function found in Functions.ml:5
  Function found in Functions.ml:5
  Function found in Functions.ml:10
  Function found in Functions.ml:10
  Function found in Functions.ml:13
  Function found in Functions.ml:16
  There are no more than 9 functions in _build/default/Functions.ml
