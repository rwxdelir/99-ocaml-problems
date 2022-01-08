let split xs num = 
  let rec aux li i acc = 
    match li with
    | [] -> List.rev acc, [] 
    | h :: t -> if num = i then List.rev acc, t
                else aux t (i + 1) (h :: acc)
  in aux xs 0 [] 
