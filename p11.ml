let encode (xs: 'a list): 'rle list =
  let rec aux li acc len =
    match li with
    | a :: (b :: _ as t) -> if a = b then aux t acc (len+1)
                            else if len = 1 
                            then aux t ((One a)::acc) 1
                            else aux t (Many(len, a)::acc) 1 
    | h :: _ -> if len > 1 then List.rev (Many(len,h)::acc) else List.rev (One h::acc)
  in aux xs [] 1 
