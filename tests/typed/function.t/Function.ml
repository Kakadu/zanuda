let should_give_a_lint x = match x with [] -> 1 | _ -> 2

let should_NOT_give_a_lint x = match x with [] -> [] | _::_ -> x

let _ = fun c ->
  match c with
  | xs -> List.mem (fun c -> c = 'a') xs
;;

let backslash = fun ch ->
  match ch with
  | ch when List.mem ch [ "$"; "'"; "\""; "\\"; "\n" ] -> ch
  | "\n" -> ""
  | ch -> "" ^ ch
;;

let _ = 
  let (let*)  =Option.bind in 
  let fail _ = None in 
  let* _res =
    Fun.id (fun error ->
        match error with
        | _ ->
          (match "" with
          | "print_newline" ->
            Some 42
          | _ -> fail error)
        ) ()
  in
  None