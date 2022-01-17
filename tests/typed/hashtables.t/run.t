  $ dune build
  $ mylinter -dir .
  File "Hashtables.ml", line 3, characters 22-23:
  3 |   if None = None then h else h
                            ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 3, characters 29-30:
  3 |   if None = None then h else h
                                   ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
