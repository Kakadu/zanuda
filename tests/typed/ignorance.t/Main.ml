let () = List.iter (fun x -> print_int x) [1; 2; 3]

module X = struct
  [@@@ocaml.warning "-unused-value-declaration"]
  [@@@ocaml.warning "-ignored-partial-application"]
  [@@@zanuda "-eta_reduction,-wrong_ignoring"]

  let () = List.iter (fun x -> print_string x) ["123"]
  let f () = ignore (List.map string_of_bool)

  [@@@zanuda "+eta_reduction"]
  let () = (fun x -> print_string x) "Eta-reduction reported again"
  [@@@zanuda "-eta_reduction"]
end

let () = (fun x -> print_string x) "Eta-reduction reported here too"
