(** Discovery of compiled modules for CRAP analysis.

    The crapper expects a compiled dune repository with all [.cmt] files in
    place, so discovery walks the build directory looking for them instead of
    re-parsing source files. Test code is not scored and is skipped. *)

(* Directories that never contain scored sources *)
let skip_dirs =
  [ ".git"; ".hg"; ".svn"; ".metrics"; "coverage"; "install"; "test"; "tests"; "spec" ]
;;

(* Hidden directories are build artifacts (dune's [.*.objs] byte directories,
   test sandboxes and so on). Only [.*.objs] contains code worth scoring. *)
let is_skip_dir name =
  List.mem name skip_dirs
  || (String.starts_with ~prefix:"." name && not (String.ends_with ~suffix:".objs" name))
;;

let index_of s sub =
  let ls = String.length s
  and lt = String.length sub in
  let rec go i =
    if i + lt > ls then None else if String.sub s i lt = sub then Some i else go (i + 1)
  in
  go 0
;;

let ends_with_tests s = String.ends_with ~suffix:"_tests" s

(* [test_helper], [bar_test] and friends should not be scored *)
let is_test_module module_name =
  String.starts_with ~prefix:"test_" module_name
  || String.ends_with ~suffix:"_test" module_name
  || ends_with_tests module_name
;;

let is_test_file name =
  let module_name =
    if String.ends_with ~suffix:".cmt" name
    then String.sub name 0 (String.length name - 4)
    else name
  in
  (* dune's wrapped libraries compile modules as [some_lib__SomeModule] *)
  match index_of module_name "__" with
  | Some i ->
    let lib = String.sub module_name 0 i in
    let m =
      String.lowercase_ascii
        (String.sub module_name (i + 2) (String.length module_name - i - 2))
    in
    (* an empty module part is dune's synthetic library wrapper *)
    m = "" || ends_with_tests lib || is_test_module m
  | None -> is_test_module module_name
;;

(* Dune puts test code into [*\_tests] directories and libraries *)
let is_test_path path =
  List.exists
    (fun part ->
      let part =
        if String.ends_with ~suffix:".objs" part
        then String.sub part 0 (String.length part - 5)
        else part
      in
      String.ends_with ~suffix:"_tests" part || String.starts_with ~prefix:"test_" part)
    (String.split_on_char '/' path)
;;

let is_cmt_file name =
  String.ends_with ~suffix:".cmt" name
  (* Module names never contain a dash; dune's generated [*\.ml-gen] rules do *)
  && not (String.contains name '-')
;;

let rec collect path acc =
  if is_test_path path
  then acc
  else if not (Sys.file_exists path)
  then acc
  else if Sys.is_directory path
  then (
    let base = Filename.basename path in
    if base <> path && is_skip_dir base
    then acc
    else (
      let entries = Array.to_list (Sys.readdir path) |> List.sort String.compare in
      List.fold_left (fun acc name -> collect (Filename.concat path name) acc) acc entries))
  else if
    is_cmt_file (Filename.basename path) && not (is_test_file (Filename.basename path))
  then path :: acc
  else acc
;;

(* Source paths given on the command line are rewritten into the build tree:
   [src] means [_build/default/src] when the project is compiled. *)
let normalize_root root =
  if String.starts_with ~prefix:"_build" root
  then root
  else (
    let root = if root = "." then "" else root in
    let candidate =
      if root = "" then "_build/default" else Filename.concat "_build/default" root
    in
    if Sys.file_exists candidate then candidate else if root = "" then "." else root)
;;

let discover_cmt roots =
  let roots = List.map normalize_root roots in
  let found = List.fold_left (fun acc root -> collect root acc) [] roots in
  List.sort_uniq String.compare (List.rev found)
;;

let default_roots = [ "_build" ]
