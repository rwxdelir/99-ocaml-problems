let duplicate xs =
  let rec aux li acc =
    match li with
    | [] -> acc 
    | h :: [] -> h :: (h :: acc)
    | h :: t -> aux t (h :: (h :: acc))
  in List.rev (aux xs [])
