let is_palindrome (xs : 'a list) : bool = 
  let rec aux x acc =
    match x with
      | [] -> acc = xs
      | head :: tail -> aux tail (head :: acc) 
  in aux xs []
