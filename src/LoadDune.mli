val analyze_dir
  :  untyped:(string -> unit)
  -> cmt:(string -> Typedtree.structure -> unit)
  -> cmti:(string -> Typedtree.signature -> unit)
  -> string
  -> unit
