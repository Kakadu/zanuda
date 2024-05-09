let f _ =
  try raise Not_found
with _ -> 1

let foo f =
  match f () with
  | [] -> 1
  | exception _ -> 2
  | _ -> 3

[@@@ocaml.warning "-unused-value-declaration" ]
[@@@ocaml.warning "-unused-var-strict"]
let bar =
  try 1 with | e -> 2
(* TODO: this should be reported too
   https://github.com/semgrep/semgrep/issues/10193
   Issue #52
   *)



module FOO(_ : sig end) = struct
  let _inside_functor =
    try 1 with | _ -> 2
end