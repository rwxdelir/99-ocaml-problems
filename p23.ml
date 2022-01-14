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
  let unique_list = unique (Random.int (List.length xs)) n [] in
  
  (*
  let rec get_element li i =
    match li with
    | [] -> raise Not_found
    | h :: t -> if i != 0 then get_element t (i-1) else h
  
  *)


 (*
  let rec aux num acc len =
    if num != 0 then aux (num-1) ((get_element xs (Random.int (len-1)))::acc) len else acc
  in aux n [] (List.length xs)
*)

