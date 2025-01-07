let f cond = function
  | [] -> if cond then 1 else 2
  | _ -> 3

