let should_give_a_lint scru =
  match scru with
  | (_, _) -> true

let should_NOT_give_a_lint scru =
  match scru with
  | (0, 1) -> true
  | (_, _) -> false
