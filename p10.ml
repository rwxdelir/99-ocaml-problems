let encode (xs: 'a list) = 
  let rec aux li acc len =
    match li with
    | [] -> []
    | a :: (b :: _ as t) -> if a = b then aux t acc (len+1) 
                            else aux t ((len, a)::acc) 1
    | h :: _ -> List.rev ((len, h)::acc)
  in aux xs [] 1
