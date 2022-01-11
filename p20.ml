let remove_at n xs = 
  let rec aux i li acc = 
    match li with
    | [] -> acc
    | h :: t -> if n != i then aux (i+1) t (h::acc) 
                else aux (i+1) t acc 
  in List.rev (aux 0 xs [])
