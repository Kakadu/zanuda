(* should give a lint *)
module _ = struct
  type t1 =
  | A of
      { a : t2
      ; b : t3
      }
  and t2 = B of t3
  and t3 = C of int

  type t4 = D of t1
end
 
(* should NOT give a lint *)
module _ = struct
  type t1 = A of t2
  and t2 = B of t1

  type t3 = C of t1
end

(* should give a lint *)
module _ = struct
  type t1 = A of t2
  and t2 = B of t1
  and t3 = C of t1
end

(* should give a lint *)
module _ = struct
  type t1 = A of t2 * t3
  and t2 = B of t1

  and t3 = C of t4 * t6
  and t4 = D of t3

  and t5 = E of int * t6 * t7
  and t6 =
    | F of t5
    | L of
        { a : int
        ; b : string
        }
    | K of int
    | O of string

  and t7 = G of string
  and t8 = H of t7
  and t9 = int
end

(* should give a lint *)
module _ = struct
  type mt1 =
  | U of t5
  | V of t2
  | W of mt2

  and mt2 =
    | X of t3
    | Y of mt1

  and t1 = A of t2
  and t2 = B of t3
  and t3 = C of t4
  and t4 = D of t5
  and t5 = E of t6
  and t6 = F of int
end

(* should give a lint*)
module _ = struct
  type nonrec a = A
  and b = B
end
