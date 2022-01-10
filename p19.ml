let rotate xs n =
  let actual_n n = if n >= 0 then n else ((List.length xs) + n) in
  let rec aux li acc i d =
    match li with 
    | [] -> acc
    | h :: t -> if i = d then aux (List.rev t) (List.rev (h :: acc)) (i+1) d
                else aux t (h :: acc) (i+1) d
  in aux xs [] 1 (actual_n n)
