type 'a lvls = { value : 'a; }
[@@deriving show { with_path = false }]

type ty = A of typ
and typ = ty lvls  [@@deriving show { with_path = false }]


type hack = int list