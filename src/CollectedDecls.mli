val add_just_decl : string -> unit
val add_used_decl : string -> unit
val print_all_decls : unit -> unit
val print_used_decls : unit -> unit
val collect_unused : unit -> unit
val collect_from_mli_tree : LoadDune.w -> string -> Typedtree.signature -> unit
