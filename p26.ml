(* https://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n *)
let extract n xs =
  let rec aux li acc =
    if List.length acc = n then [List.rev acc]
    else match li with
         | [] -> []
         | h :: t -> aux t (h::acc) @ aux t acc
  in aux xs []
