  $ dune build
  $ ../zanuda.exe -no-top_file_license -dir . -ordjsonl /dev/null | sed '/^[[:space:]]*$/d'
  File "Main.ml", line 1, characters 19-41:
  1 | let () = List.iter (fun x -> print_int x) [1; 2; 3]
                         ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun x -> print_int x' as 'print_int'
  File "Main.ml", line 9, characters 13-45:
  9 |   let f () = ignore (List.map string_of_bool)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Unsafe ignore. It's recommended to rewrite it as 'let (_: bool list -> string list) = List.map string_of_bool'
  File "Main.ml", line 12, characters 11-36:
  12 |   let () = (fun x -> print_string x) "Eta-reduction reported again"
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Eta reduction proposed. It's recommended to rewrite 
                       'fun x -> print_string x' as 'print_string'
