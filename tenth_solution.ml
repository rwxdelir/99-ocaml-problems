(* Incorrect result: first element of 'a list list 
   always doesn't contain one element *)
let pack (xs : 'a list) = 
  let rec aux l tmp ans =  
    match l with
    | a :: (b :: _ as tl) -> if a = b 
                            then aux tl (a :: tmp) ans
                            else aux tl [] ((a :: tmp) :: ans) 
    | result -> tmp::ans
  in aux xs [] [] 
