module Pattern = struct
  type t = int [@@deriving show { with_path = false }]
end
type value_binding = { pattern : Pattern.t }
[@@deriving  show { with_path = false }]