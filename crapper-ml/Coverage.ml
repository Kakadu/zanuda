(** Coverage from bisect_ppx.

    The [.coverage] files written by the bisect runtime are parsed directly:
    the [BISECT-COVERAGE-4] format is a length-prefixed, space-separated dump
    of every instrumented source file with the byte offsets of its points and
    their visit counts. Multiple runs are merged by summing the counts. A
    function is covered through its byte span in the source file: the same
    offsets the extraction step recorded. *)

type point =
  { offset : int
  ; count : int
  }

(* Per source file: filename -> points *)
type data = (string * point list) list

let identifier = "BISECT-COVERAGE-4"
let identifier_len = String.length identifier

(* every integer and string is preceded by a space, and so is the length of
   every array *)
let parse_file content =
  let fail () = invalid_arg "Bisect coverage file" in
  let len = String.length content in
  let pos = ref 0 in
  let skip_space () =
    if !pos >= len || content.[!pos] <> ' ' then fail () else incr pos
  in
  let read_int () =
    skip_space ();
    let start = !pos in
    while !pos < len && content.[!pos] >= '0' && content.[!pos] <= '9' do
      incr pos
    done;
    if !pos = start then fail ();
    int_of_string (String.sub content start (!pos - start))
  in
  let read_string () =
    let n = read_int () in
    skip_space ();
    if !pos + n > len then fail ();
    let s = String.sub content !pos n in
    pos := !pos + n;
    s
  in
  let read_array read_element () =
    let n = read_int () in
    List.init n (fun _ -> read_element ())
  in
  if len < identifier_len || String.sub content 0 identifier_len <> identifier
  then fail ();
  pos := identifier_len;
  let n_files = read_int () in
  List.init n_files (fun _ ->
    let filename = read_string () in
    let points = read_array read_int () in
    let counts = read_array read_int () in
    if List.length points <> List.length counts then fail ();
    filename, List.map2 (fun offset count -> { offset; count }) points counts)
;;

(* Several test runs write their own [.coverage] file; sum the counts. *)
let merge (datas : data list) : data =
  let per_file = Hashtbl.create 17 in
  List.iter
    (List.iter (fun (filename, points) ->
       let per_offset =
         match Hashtbl.find_opt per_file filename with
         | Some tbl -> tbl
         | None ->
           let tbl = Hashtbl.create 17 in
           Hashtbl.add per_file filename tbl;
           tbl
       in
       List.iter
         (fun { offset; count } ->
           let old =
             match Hashtbl.find_opt per_offset offset with
             | Some old -> old
             | None -> 0
           in
           Hashtbl.replace per_offset offset (old + count))
         points))
    datas;
  Hashtbl.fold
    (fun filename per_offset acc ->
      ( (filename : string)
      , Hashtbl.fold (fun offset count acc -> { offset; count } :: acc) per_offset [] )
      :: acc)
    per_file
    []
;;

let collect dir =
  if Sys.file_exists dir && Sys.is_directory dir
  then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".coverage")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)
  else []
;;

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;
  content
;;

(* [BISECT-COVERAGE-4] recorded the source path as the compiler saw it; look
   it up by that name first, then by the longest matching path suffix. *)
let lookup (f : Model.function_) (data : data) =
  match List.assoc_opt f.Model.path data with
  | Some _ as exact -> exact
  | None ->
    let suffix_length (name, _) =
      if
        Filename.check_suffix name f.Model.path || Filename.check_suffix f.Model.path name
      then min (String.length name) (String.length f.Model.path)
      else 0
    in
    let best =
      List.fold_left
        (fun (best, best_len) entry ->
          let n = suffix_length entry in
          if n > best_len then Some (snd entry), n else best, best_len)
        (None, 0)
        data
    in
    fst best
;;

(* The function is covered if its byte span in the source file contains
   instrumented points; a function the report does not mention scores 0%. *)
let coverage_of (f : Model.function_) (data : data) =
  match lookup f data with
  | None -> Some 0.0
  | Some points ->
    (match
       List.filter
         (fun { offset; _ } -> f.Model.start_byte <= offset && offset <= f.Model.end_byte)
         points
     with
     | [] -> Some 0.0
     | in_range ->
       let hits = List.length (List.filter (fun { count; _ } -> count > 0) in_range) in
       Some (100.0 *. float_of_int hits /. float_of_int (List.length in_range)))
;;

(* Directory where bisect writes its files and the runner keeps the
   accumulated report. *)
let coverage_dir = ".metrics/coverage"
let default_coverage_command = "dune runtest --instrument-with bisect_ppx --force"

(* A fresh run deletes the previous report, like the reference crapper *)
let prepare dir =
  let parent = Filename.dirname dir in
  if parent <> "." && not (Sys.file_exists parent) then Sys.mkdir parent 0o755;
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".coverage")
  |> List.iter (fun name -> Sys.remove (Filename.concat dir name))
;;

let load ~use_existing ~command =
  if not use_existing
  then (
    prepare coverage_dir;
    (* absolute: test processes run inside dune's sandbox directories *)
    let bisect_prefix = Filename.concat (Sys.getcwd ()) coverage_dir in
    let cmd =
      Printf.sprintf "BISECT_FILE=%s/bisect %s" (Filename.quote bisect_prefix) command
    in
    if Sys.command cmd <> 0
    then prerr_endline "crapper: coverage command failed; functions are scored at 0%");
  let data = collect coverage_dir |> List.map (fun path -> parse_file (read_file path)) in
  merge data
;;
