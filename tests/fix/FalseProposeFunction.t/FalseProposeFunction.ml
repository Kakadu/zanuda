(* Linter doesn't correctly recognize an annotation as a expression containing a pattern matching *)
type private_flag =
  | Private
  | Public
[@@deriving eq, show { with_path = false }]

type closed_flag =
  | Closed
  | Open
[@@deriving eq, show { with_path = false }]
