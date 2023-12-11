let group_list_by ?initial_size l =
  match initial_size with
  | Some size -> (string_of_int size) :: l
  | None -> [""]
;;