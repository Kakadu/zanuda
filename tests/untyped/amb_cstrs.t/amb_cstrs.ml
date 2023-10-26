module Linted = struct
  type 'a result = 
    | Error of string 
    | Ok of 'a  

  type str_option = 
    | Some of string
    | None

  type 'a option = 
    | Nothing
    | Some of 'a
end

module Not_linted = struct
  type 'a option = 
    | Something of 'a 
    | Nothing
end