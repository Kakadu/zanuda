(* A test for issue #45 *)
module Let_syntax = struct
  let bind x ~f = Option.bind x f
end

let _ =
  let%bind res = Option.some 42 in
  match res with
  | _ -> None