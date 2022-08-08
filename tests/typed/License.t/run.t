  $ dune build
  $ zanuda -dir .
  File "bad2.ml", line 2, characters 0-42:
  2 | (** Copyright , Kakadu and contributors *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad2.ml", line 4, characters 0-51:
  4 | (** SPDX-License-Identifier Semi-colon-forgotten *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with corrent license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
  File "bad.ml", line 1, characters 0-9:
  1 | let _ = 5
      ^^^^^^^^^
  Alert zanuda-linter:  First item in file should be a documentation comment with copyright information. For example:
              (** Copyright 2021-2022, Winnie Pooh et al. *)
  File "bad.ml", line 2, characters 0-9:
  2 | let _ = 5
      ^^^^^^^^^
  Alert zanuda-linter: Second item in file should be a documentation comment with corrent license information. For example:
             (** SPDX-License-Identifier: LGPL-3.0-or-later *)
