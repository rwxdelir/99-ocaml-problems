let length_sort xs =
  let rec get li i j =
    match li with
    | [] -> raise Not_found
    | h :: t -> if i != j then get t (i+1) j else h
  in 
  let rec ans li acc =
    if List.length acc = List.length xs then acc
    else match li with
         | [] -> acc
         | h :: t -> ans t ((get xs 0 (List.nth h 1))::acc)
  in
  let rec aux li acc i =
    match li with
    | [] -> List.rev (ans (List.sort compare acc) [])
    | h :: t -> aux t ((List.length h::(i::[]))::acc) (i+1)
  in aux xs [] 0 
 
