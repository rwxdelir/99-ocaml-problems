let rand_select xs n =
  let rec check li x =
    match li with
    | [] -> true
    | h :: t -> if x = h then false else check t x
  in
  let rec unique x i li =
    if (check li x) && i > 0 then unique (Random.int (List.length xs)) (i-1) (x::li)
    else if i > 0 then unique (Random.int (List.length xs)) i li
    else li
  in  
  let rec get_element li i =
    match li with
    | [] -> raise Not_found
    | h :: t -> if i != 0 then get_element t (i-1) else h
  in
  let unique_list = unique (Random.int (List.length xs)) n [] in
  let rec aux ind acc =
    match ind with
    | [] -> acc
    | h :: t -> aux t ((get_element xs h) :: acc)
  in aux unique_list []
  
