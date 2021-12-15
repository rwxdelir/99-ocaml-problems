let rec at n xs =
  match xs with
  | [] -> None
  | head :: tail -> if n = 1 then Some head else at (n - 1) tail;;

let () = 
  print_endline "at()";
