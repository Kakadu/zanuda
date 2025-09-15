  $ dune build @default @check ./REPL.bc

  $ ../zanuda.exe -no-check-filesystem -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "REPL.ml", line 1, characters 0-9:
  1 | open List
      ^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "REPL.ml", line 3, characters 0-11:
  3 | open Option
      ^^^^^^^^^^^
  Alert zanuda-linter: OCaml files should provide license information in second line (structure item)
  File "lib/empty.mli", line 1:
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad.ml", line 1, characters 0-9:
  1 | let _ = 5
      ^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad.ml", line 2, characters 0-9:
  2 | let _ = 6
      ^^^^^^^^^
  Alert zanuda-linter: OCaml files should provide license information in second line (structure item)
  File "bad2.ml", line 2, characters 0-42:
  2 | (** Copyright , Kakadu and contributors *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad2.ml", line 4, characters 0-51:
  4 | (** SPDX-License-Identifier Semi-colon-forgotten *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: OCaml files should provide license information in second line (structure item)
  File "lib/asdf.ml", line 5, characters 0-52:
  5 | (** SPDX-LicenBUGse-Identifier: LGPL-3.0-or-later *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "lib/asdf.mli", line 1, characters 0-13:
  1 | val foo : int
      ^^^^^^^^^^^^^
  Alert zanuda-linter: OCaml files should provide license information in second line (structure item)
