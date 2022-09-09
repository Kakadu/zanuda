type chunk_info =
  { old : int
  ; old_range : int
  ; fresh : int
  ; fresh_range : int
  }
[@@deriving show]

type pos = int [@@deriving show]
type chunk = chunk_info * ([ `Add | `Del | `Leave ] * string * pos) list [@@deriving show]

type file_info =
  { old_file : string
  ; new_file : string
  ; chunks : chunk list
  }
[@@deriving show]
