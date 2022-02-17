let func1 () = () (* 1 *)

let func2 _ = () (* 2 *)

let func3 a1 a2 a3 = a1 (); a2 (); a3 (); () (* 3, 4, 5 *)

let not_func1 = "Not a function"

let not_func2 = 
  fun a b -> a (); b (); () (* 6, 7 *)

let not_func3 = 
  function a -> a (); () (* 8 *)

let not_func4 =
  let func4 _ = () in (* 9 *)
  func4 "tmp"

let not_func5 =
  let not_func6 = () in
  not_func6
