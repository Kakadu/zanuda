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
  type 'a opt = 
    | Something of 'a 
    | Nothing

  type 'a maybe = 'a option = None | Some of 'a 

  type ('c, 'd) res = ('c, 'd) result = Ok of 'c | Error of 'd
end

(* should give a lint *)
type 'a option = 
  | Option
  | Some of 'a

(* should not give a lint *)
type 'a option2 = 'a option = 
  | Option
  | Some of 'a