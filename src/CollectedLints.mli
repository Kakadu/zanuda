val clear : unit -> unit
val is_empty : unit -> bool
val add : loc:Warnings.loc -> (module LINT.REPORTER) -> unit
val report : unit -> unit
