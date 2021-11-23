  $ dune build
  $ mylinter -dir .
  File "Hashtables.ml", line 3, characters 22-23:
  3 |   if None = None then h else h
                            ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. You should try to use standart tree-like maps. If you really want to use hashing consider Hash-Array Mapped Tries (HAMT)
  File "Hashtables.ml", line 3, characters 29-30:
  3 |   if None = None then h else h
                                   ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. You should try to use standart tree-like maps. If you really want to use hashing consider Hash-Array Mapped Tries (HAMT)
