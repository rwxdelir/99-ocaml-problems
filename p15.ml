let replicate xs num =
  let rec counter acc n h =
    if n > 0 then counter (h::acc) (n-1) h else acc in
  let rec aux li acc = 
    match li with
    | [] -> acc
    | h :: t -> aux t (counter acc num h) 
  in List.rev (aux xs [])
