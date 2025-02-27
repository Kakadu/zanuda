let xxx = [%blob "a.ts"]
(*
module _ = struct
  open Protocol_conv_xmlm

  let _ = print_endline xxx

  type b =
    | A of int
    | B of int [@key "b"]
    | C
  [@@deriving protocol ~driver:(module Xmlm)]

  let __ _ = Format.printf "%s\n%!" (Xmlm.to_string_hum (b_to_xmlm (B 5)))

  type item =
    { source : string
    ; translatorcomment : string option
    ; translation : string
    }
  [@@deriving protocol ~driver:(module Xmlm)]

  open Base
  open Stdlib.Format

  let () =
    printf
      "%s\n%!"
      (Xmlm.to_string_hum
         (item_to_xmlm
            { source = "adf"; translatorcomment = None; translation = "trans" }))
  ;;

  let _ =
    let _, nodes = Ezxmlm.from_string xxx in
    let on_context = function
      | `El (((_, "name"), _), [ `Data s ]) -> printf "name %s" s
      | _ -> ()
    in
    let on_ts = function
      | `El (((_, "context"), _attrs), nodes) -> List.iter ~f:on_context nodes
      | _ -> ()
    in
    List.iter nodes ~f:(function
        | `Data _ -> ()
        | `El (((_, "TS"), _attrs), nodes) -> List.iter ~f:on_ts nodes
        | _ -> ())
  ;;
end
*)
module _ = struct
  (* open Xmlm *)

  type w3c_bureaucrat =
    { name : string
    ; surname : string
    ; honest : bool
    ; obfuscation_level : float
    ; trs : string list
    }

  let _in_w3c_bureaucrats src =
    let i = Xmlm.make_input ~strip:true src in
    let tag n = ("", n), [] in
    let error () = invalid_arg "parse error" in
    let accept s i = if Xmlm.input i = s then () else error () in
    let rec i_seq el acc i =
      match Xmlm.peek i with
      | `El_start _ -> i_seq el (el i :: acc) i
      | `El_end -> List.rev acc
      | _ -> error ()
    in
    let i_el n i =
      accept (`El_start (tag n)) i;
      let d =
        match Xmlm.peek i with
        | `Data d ->
          ignore (Xmlm.input i);
          d
        | `El_end -> ""
        | _ -> error ()
      in
      accept `El_end i;
      d
    in
    let i_bureaucrat i =
      try
        accept (`El_start (tag "bureaucrat")) i;
        let name = i_el "name" i in
        let surname = i_el "surname" i in
        let honest =
          match Xmlm.peek i with
          | `El_start (("", "honest"), []) ->
            ignore (i_el "honest" i);
            true
          | _ -> false
        in
        let obf = float_of_string (i_el "obfuscation_level" i) in
        let trs = i_seq (i_el "tr") [] i in
        accept `El_end i;
        { name; surname; honest; obfuscation_level = obf; trs }
      with
      | Failure _ -> error ()
      (* float_of_string *)
    in
    accept (`Dtd None) i;
    accept (`El_start (tag "list")) i;
    let bl = i_seq i_bureaucrat [] i in
    accept `El_end i;
    if not (Xmlm.eoi i) then invalid_arg "more than one document";
    bl
  ;;

  open Base
  module Format = Stdlib.Format
  open Format
  open Sexplib.Sexp



  type message =
    { src : string
    ; comment : string option
    ; tr : string
    } [@@deriving sexp]

  type context = string * message list

  let parse_ts src : context =
    let open Printf in
    let i = Xmlm.make_input ~strip:true src in
    let tag n = ("", n), [] in
    let error () =
      let l, c = Xmlm.pos i in
      invalid_arg (sprintf "parse error: %d %d" l c)
    in
    let accept s i = if Caml.(Xmlm.input i = s) then () else error () in
    let rec i_seq el acc i =
      match Xmlm.peek i with
      | `El_start _ -> i_seq el (el i :: acc) i
      | `El_end -> List.rev acc
      | _ -> error ()
    in
    let maybe_dtd i =
      match Xmlm.peek i with
      | `Dtd _ ->
        let _ = Xmlm.input i in
        ()
      | _ -> ()
    in
    let i_el n i =
      accept (`El_start (tag n)) i;
      let d =
        match Xmlm.peek i with
        | `Data d ->
          ignore (Xmlm.input i);
          d
        | `El_end -> ""
        | _ -> error ()
      in
      accept `El_end i;
      d
    in
    let any_data i =
      match Xmlm.peek i with
      | `Data s ->
        let _ = Xmlm.input i in
        s
      | `El_start ((_, s), _) ->
        printf "got %s but data expected\n%!" s;
        error ()
      | _ -> error ()
    in
    let i_translation i =
      match Xmlm.peek i with
      | `El_start ((_, "translation"), _) ->
        let _ = Xmlm.input i in
        printf "%s %d\n%!" __FILE__ __LINE__;
        let tr = any_data i in
        printf "%s %d\n%!" __FILE__ __LINE__;
        accept `El_end i;
        tr
      (* | `El_start ((_, "translation"), [ ((_, "type"), v) ]) ->
        let tr = any_data i in
        accept `El_end i;
        tr *)
      | _ -> error ()
    in
    let i_message i =
      accept (`El_start (tag "message")) i;
      let src = i_el "source" i in
      let cmt = i_el "translatorcomment" i in
      let tr = i_translation i in
      (* print_endline _tr; *)
      accept `El_end i;
      { src; comment = Some cmt; tr }
    in
    let context i =
      accept (`El_start (tag "context")) i;
      let name = i_el "name" i in
      let msgs = i_seq i_message [] i in
      accept `El_end i;
      name, msgs
    in
    let ts i =
      match Xmlm.input i with
      | `El_start ((_, "TS"), _) ->
        let rez = context i in
        accept `El_end i;
        rez
      | _ -> error ()
      (* accept (`El_start (tag "TS")) i; *)
    in
    (* accept (`Dtd None) i; *)
    maybe_dtd i;
    let bl = ts i in
    (* accept (`El_start (tag "ts")) i;
    let bl = i_seq i_bureaucrat [] i in
    accept `El_end i; *)
    if not (Xmlm.eoi i) then invalid_arg "more than one document";
    bl
  ;;

  let _ = parse_ts (`String (0, xxx))
end
