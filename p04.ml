let length (xs : 'a list) : int =
  let rec helper xs n =
    match xs with
    | [] -> n
    | _ :: tail -> helper tail (n + 1)
  in helper xs 0
