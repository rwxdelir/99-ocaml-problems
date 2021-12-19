let pack (xs : 'a list) = 
  let rec aux l tmp ans =  
    match l with
    | [] -> []
    | a :: (b :: _ as tl) -> if a = b 
                            then aux tl (a :: tmp) ans
                            else aux tl [] ((a :: tmp) :: ans) 
    | head :: _ -> List.rev ((head::tmp)::ans)
  in aux xs [] [] 
