type t =
  | App of t * t
  | Var of string
  (** A  variable in ULC *)
