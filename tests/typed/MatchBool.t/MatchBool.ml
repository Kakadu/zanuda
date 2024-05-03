let bool_match1 a = match a with 
  | true -> 1
  | false -> 2


let bool_match2 a = match a with
  | false -> (
    Format.printf "meow";
    1)
  | true -> 2
