let extract n xs =
  let rec get el i li tmp acc = 
    match li with
    | [] -> if (List.length tmp) = n then tmp::acc else acc 
    | h :: t -> if i != n then get el (i+1) t (h::tmp) acc
                else get el 1 li [el] (tmp::acc)
  in get "a" 1 ["b"; "c"; "d"] ["a"] [] 
  (*
  let rec aux list acc =
    match list with
    | [] -> acc 
    | h :: [] -> acc
    | h :: t -> aux t ((get h 1 t [h] [])::acc)
  in aux (List.rev xs) []
*)
  



