type 'a node =
  | One of 'a 
  | Many of 'a node list;; 

let flatten xs =
  let rec aux ans li =
    match li with 
    | [] -> ans 
    | One x :: tail -> aux (x::ans) tail 
    | Many y::tail -> aux (aux ans y) tail 
  in List.rev (aux [] xs);;
