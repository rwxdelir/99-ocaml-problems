let reverse (xs : 'a list) : 'a list =
  let rec aux xs tmp =
    match xs with
      [] -> tmp
    | h :: t -> aux t (h :: tmp)
  in
    aux xs []
