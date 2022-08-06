  $ dune build
  $ zanuda -dir .
  File "Hashtables.ml", line 3, characters 22-23:
  3 |   if None = None then h else h
                            ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 3, characters 29-30:
  3 |   if None = None then h else h
                                   ^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 11, characters 19-26:
  11 | type _t1 = { xxx : int ref }
                          ^^^^^^^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 12, characters 19-31:
  12 | type _t2 = { xxx : int Base.ref }
                          ^^^^^^^^^^^^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 17, characters 14-27:
  17 |   let code = !last_identity in
                     ^^^^^^^^^^^^^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
  File "Hashtables.ml", line 18, characters 2-15:
  18 |   last_identity := code + 1;
         ^^^^^^^^^^^^^
  Alert zanuda-linter: Using mutable data structures for teaching purposes is usually discouraged. Replace Hashtables by standart tree-like maps or consider Hash-Array Mapped Tries (HAMT). Use mutable `ref`erences and mutable structure fields only if it is really required. In all places where it is needed indeed, describe in a comment why it is needed there.
