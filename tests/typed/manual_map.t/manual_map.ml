(* Should give a lint *)
let rec map1 f = function 
  | [] -> []
  | h :: tl -> f h :: map1 f tl
;;

let rec map2 f = function 
  | x :: xs -> f x :: map2 f xs
  | [] -> []
;;

let rec map3 = function 
  | [] -> []
  | h :: tl -> h + 1 :: map3 tl
;;
let rec map4 f l = match l with
  | [] -> []
  | x :: xs -> f x :: map4 f xs
;;

let rec map5 l f = match l with
  | [] -> []
  | x :: xs -> f x :: map5 xs f
;;

let concat input = 
  let rec map = function
    | x :: xs -> Int.to_string x :: map xs
    | [] -> []
  in
  map input
;;
