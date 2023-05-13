(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** A type of lint's impelementation. Typed and untyped lints inspect OCaml's Typedtree/Parsetree respectively. *)
type impl =
  | Typed
  | Untyped

(** Group of lints. The same as Rust's Clippy *)
type group =
  | Style
  | Correctness
  | Perf
  | Restriction
  | Deprecated
  | Pedantic
  | Complexity
  | Suspicious
  | Nursery

(** Level of lints. The same as Rust's Clippy *)
type level =
  | Allow
  | Warn
  | Deny
  | Deprecated

(** How various lints were invented *)
type lint_source =
  | Camelot (** Adopted from Camelot linter *)
  | Clippy (** Adopted from Rust's Clippy *)
  | FPCourse (** Invented after reviewing Kakadu's student's OCaml homeworks *)
  | Other (** The source is not specified *)

module type GENERAL = sig
  type input

  (** Linter id. Should be unique *)
  val lint_id : string

  (** How this lint appeared. *)
  val lint_source : lint_source

  val level : level

  (** Run this lint and save result in global store {!CollectedLints}. *)
  val run : Compile_common.info -> input -> input

  (** Lint's documentation in Markdown.*)
  val documentation : string

  (** Dump lint's documentation as Markdown.*)
  (* val describe_as_markdown : unit -> Yojson.Safe.t *)

  (** Dump lint's documentation to as JSON object to use in web-based interfaces. *)
  val describe_as_json : unit -> Yojson.Safe.t
end

(* In this design we can't define a linter that processes both parsetree and typedtree. Is it important? *)

module type UNTYPED = sig
  type input = Ast_iterator.iterator

  include GENERAL with type input := input
end

module type TYPED = sig
  type input = Tast_iterator.iterator

  include GENERAL with type input := input
end

module type REPORTER = sig
  (** *)

  (** Report lint info as text. Useful to print to terminal. *)
  val txt : Format.formatter -> unit -> unit

  (** Report lint info as RDJSONL format.

    Originally, it was used with ReviewDog tool to publish Github review.
    But nowadays we are abandoning it.

    @deprecated
  *)
  val rdjsonl : Format.formatter -> unit -> unit
end
