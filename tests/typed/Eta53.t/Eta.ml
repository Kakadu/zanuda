let my_id x = x

let wrapper x = my_id x

let my_add x y = x + y

let my_add3 x y z = x + y + z

let good_wrapper x = my_add x x

let strange_wrapper x x = my_add x x [@@ocaml.warning "-27"]

let xx f g h = my_add3 f g h

let flipper x y z = my_add3 y z x

let listsAreEqual a b = List.equal (fun lhs rhs -> String.equal lhs rhs) a b

let labeled_add ~x ~y = x + y

let labeled_wrapper a b = labeled_add ~x:a ~y:b
