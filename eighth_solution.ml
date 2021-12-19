let rec compress (xs: 'a list): 'a list =
  match xs with 
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t 
  | result -> result;;

