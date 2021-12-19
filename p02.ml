let rec last_two (xs: 'a list) =
  match xs with 
  | [] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> last_two rest

let () =
  print_endline "last_two()"
