let f cond = function
  | [] -> if cond then 1 else 2
  | _ -> 3

let f_noarg = function
  | [x] -> if x>0 then 1 else 2
  | _ -> 3

