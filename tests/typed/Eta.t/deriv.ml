type 'a lvls = { value : 'a; }
[@@deriving show { with_path = false }]

type ty = A of typ
and typ = ty lvls  [@@deriving show { with_path = false }]


type hack = int list
 
(** In the below definition eta conversion is possible because of deriving.eq 
The reported located is a constructor, not the type definition
*)
type expr = FuncCall of expr list [@@deriving eq ]

