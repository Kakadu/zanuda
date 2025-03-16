type exprA =
  | App of exprA * exprA
  | Abs of string * exprA
  | Var of string
  (** A  variable in ULC *)
