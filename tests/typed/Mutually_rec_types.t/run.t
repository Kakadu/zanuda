  $ dune build
  $ ../zanuda.exe -no-check-filesystem -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "Mutually_rec_types.ml", lines 3-9, characters 2-19:
  3 | ..type t1 =
  4 |   | A of
  5 |       { a : t2
  6 |       ; b : t3
  7 |       }
  8 |   and t2 = B of t3
  9 |   and t3 = C of int
  Alert zanuda-linter: Unneeded mutual recursion detected in these type declarations. It's recommended to rewrite 't3', 't2' as follows:
                       type t3 =
                         | C of int 
                       type t2 =
                         | B of t3 
  File "Mutually_rec_types.ml", lines 24-26, characters 2-18:
  24 | ..type t1 = A of t2
  25 |   and t2 = B of t1
  26 |   and t3 = C of t1
  Alert zanuda-linter: Unneeded mutual recursion detected in these type declarations. It's recommended to rewrite 't3' as follows:
                       type t3 =
                         | C of t1 
  File "Mutually_rec_types.ml", lines 31-49, characters 2-14:
  31 | ..type t1 = A of t2 * t3
  32 |   and t2 = B of t1
  33 | 
  34 |   and t3 = C of t4 * t6
  35 |   and t4 = D of t3
  ...
  46 | 
  47 |   and t7 = G of string
  48 |   and t8 = H of t7
  49 |   and t9 = int
  Alert zanuda-linter: Unneeded mutual recursion detected in these type declarations. It's recommended to rewrite 't7', 't5', 't6', 't3', 't4', 't8', 't9' as follows:
                       type t7 =
                         | G of string 
                       type t5 =
                         | E of int * t6 * t7 
                       and t6 =
                         | F of t5 
                         | L of {
                         a: int ;
                         b: string } 
                         | K of int 
                         | O of string 
                       type t3 =
                         | C of t4 * t6 
                       and t4 =
                         | D of t3 
                       type t8 =
                         | H of t7 
                       type t9 = int
  File "Mutually_rec_types.ml", lines 54-68, characters 2-19:
  54 | ..type mt1 =
  55 |   | U of t5
  56 |   | V of t2
  57 |   | W of mt2
  58 | 
  ...
  65 |   and t3 = C of t4
  66 |   and t4 = D of t5
  67 |   and t5 = E of t6
  68 |   and t6 = F of int
  Alert zanuda-linter: Unneeded mutual recursion detected in these type declarations. It's recommended to rewrite 't6', 't5', 't4', 't3', 't2', 't1' as follows:
                       type t6 =
                         | F of int 
                       type t5 =
                         | E of t6 
                       type t4 =
                         | D of t5 
                       type t3 =
                         | C of t4 
                       type t2 =
                         | B of t3 
                       type t1 =
                         | A of t2 
  File "Mutually_rec_types.ml", lines 73-74, characters 2-11:
  73 | ..type nonrec a = A
  74 |   and b = B
  Alert zanuda-linter: Unneeded mutual recursion detected in these type declarations. It's recommended to rewrite 'b' as follows:
                       type b =
                         | B 
