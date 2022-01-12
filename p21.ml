let rec insert_at str i li =
  match li with 
  | [] -> if i = 0 then str::li else li
  | h :: t -> if i = 0 then str :: (h :: insert_at str (i-1) t) else h :: insert_at str (i-1) t
