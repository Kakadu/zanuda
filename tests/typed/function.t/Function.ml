let should_give_a_lint x = match x with [] -> 1 | _ -> 2

let should_NOT_give_a_lint x = match x with [] -> [] | _::_ -> x
