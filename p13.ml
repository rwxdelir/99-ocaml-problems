type 'a rle =
  | One of 'a
  | Many of (int * 'a) 

let encode xs = 
  let make_type n c = if n = 0 then One c else Many (n+1, c) in 
  let rec aux li n ans =
    match li with
    | [] -> ans
    | a :: [] -> (make_type n a) :: ans
    | a :: (b :: _ as t) -> if a = b then aux t (n+1) ans
                            else aux t 0 ((make_type n a)::ans)
  in aux (List.rev xs) 0 [];;
