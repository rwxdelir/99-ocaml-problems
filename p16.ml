let drop xs n =
  let rec aux li i = 
    match li with
    | [] -> []
    | h :: t -> if n = i then aux t 1
                else h :: aux t (i+1)
  in aux xs 1
