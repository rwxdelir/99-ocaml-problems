(* This solution not completed. 
   It's not working with empty string as first argument *)
let compress (xs : 'a list) : 'a list = 
  let rec aux x acc h = 
    match x with
    | [] -> acc
    | head :: tail -> if h = head
                      then aux tail acc h 
                      else head :: aux tail acc head
  in aux xs [] ""
