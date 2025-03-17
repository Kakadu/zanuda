(**  Folds  *)

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs
;;

(** fold_left without match *)
let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_left1 f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold_left1 f (f acc x) xs
;;
let rec fold_right f acc l =
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f  acc xs)
;;

(** Not folds *)

(* Not a fold_right: bad order  *)
let rec fold_right f l acc =
  (* TODO: support swapped arguments order *)
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)
;;

let f = (+);;

let rec fold_right1 acc = function
  | [] -> acc
  | x :: xs -> f x (fold_right1 acc xs)
;;

let rec fold_right2 acc = function
  | x :: xs -> f x (fold_right2 acc xs)
  | [] -> acc
;;

let rec fold_left2 acc = function
  | x :: xs -> fold_left2 (f acc x) xs
  | [] -> acc
;;

let sum =
  (* TODO: support this *)
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> x + helper acc xs
  in helper 0




