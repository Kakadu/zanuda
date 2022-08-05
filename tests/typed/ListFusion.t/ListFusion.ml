let __ xs = List.map Fun.id (List.map Fun.id xs)
let __ f xs = List.filter f (List.map Fun.id xs)
let __ xs = List.concat (List.map Fun.id xs)
let __ xs = xs |> List.map Fun.id |> List.map Fun.id
