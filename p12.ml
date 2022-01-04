type 'a rle = 
  | One of 'a
  | Many of (int * 'a) 

let decode (xs: 'rle list) =
  let rec aux li ans = 
    match li with 
    | [] -> List.rev ans 
    | One x :: tail -> aux tail (x::ans)
    | Many (a, b) :: tail -> if a = 1 then aux tail (b::ans) 
                             else aux (Many(a-1, b)::tail) (b::ans) 
  in aux xs []
