let range x y =
  let rec aux x y =
    if x != y then x :: aux (x+1) y
    else [x]
  in if x < y then aux x y else List.rev (aux y x)
