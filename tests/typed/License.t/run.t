  $ dune build @default ./REPL.exe
$ tree _build/default -a
  $ zanuda -dir .
  File "REPL.ml", line 1, characters 0-9:
  1 | open List
      ^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "REPL.ml", line 3, characters 0-11:
  3 | open Option
      ^^^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "lib/asdf.ml", line 3, characters 0-50:
  3 | (** SPDX-Licen2se-Identifier: LGPL-3.0-or-later *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "lib/asdf.mli", line 1, characters 0-13:
  1 | val foo : int
      ^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "lib/asdf.mli", line 1:
  Alert zanuda-linter: OCaml files should start from copyright and license information
  File "bad2.ml", line 2, characters 0-42:
  2 | (** Copyright , Kakadu and contributors *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad2.ml", line 4, characters 0-51:
  4 | (** SPDX-License-Identifier Semi-colon-forgotten *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "bad.ml", line 1, characters 0-9:
  1 | let _ = 5
      ^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad.ml", line 2, characters 0-9:
  2 | let _ = 5
      ^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with correct license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
