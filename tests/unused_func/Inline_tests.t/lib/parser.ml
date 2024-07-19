let parse_to_some p str =
  match Angstrom.parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let s_declaration = Angstrom.return (Ast.Return None)
