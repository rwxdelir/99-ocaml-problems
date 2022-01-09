let slice xs a b = 
  let rec aux li acc ac bc =
    match li with 
    | [] -> acc
    | h :: t -> if ac >= a && bc <= b then aux t (h::acc) (ac+1) (bc+1) 
                else aux t acc (ac+1) (bc+1)
  in List.rev (aux xs [] 0 0) 
