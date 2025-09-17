(** Module types for program tree analyzers *)

[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** A type of lint's implementation.
    Typed and untyped lints inspect OCaml's Parsetree/Typedtree respectively.
    In Clippy it corresponds to {{:https://doc.rust-lang.org/nightly/clippy/development/lint_passes.html} early and late} lint passes. *)
type impl =
  | Untyped
  | Typed

(** Group of lints. The same as {{:https://doc.rust-lang.org/nightly/clippy} Rust's Clippy} *)
type group =
  | Style (** Code that should be written in a more idiomatic way *)
  | Correctness (** Code that is outright wrong or useless *)
  | Perf (** Code that can be written to run faster *)
  | Restriction
  | Deprecated
  | Pedantic (** Lints which are rather strict or might have false positives *)
  | Complexity
  | Suspicious
  | Nursery (** New lints that are still under development *)

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

module type UNUSED_DECLS = sig
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

      @deprecated *)
  val rdjsonl : Format.formatter -> unit -> unit
end
