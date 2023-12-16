let ( @@ ) = Core.Fn.compose

let split_on ?(trim = true) sep s =
  (if trim then String.trim s else s) |> Str.split (Str.regexp sep)
;;

let group_blocks lines =
  let add_lines_to_group blocks line =
    match blocks with
    | [] -> [ [ line ] ]
    | block :: rest ->
      if line = "" then [] :: block :: rest else (line :: block) :: rest
  in
  List.fold_left add_lines_to_group [] lines
  |> List.rev_map List.rev
  |> List.filter (( <> ) [])
;;

let lines_to_matrix lines =
  let cols = List.hd lines |> String.length in
  List.map
    (fun line ->
      let len = String.length line in
      if len <> cols
      then raise (Failure "Char matrix can't have variable-length rows.")
      else Array.of_seq (String.to_seq line))
    lines
  |> Array.of_list
;;

let floats_of_string s = split_on " +" s |> List.map float_of_string
let ints_of_string s = split_on " +" s |> List.map int_of_string
let data_col = List.hd @@ List.tl @@ split_on ":"
let float_of_nums = float_of_string @@ Str.global_replace (Str.regexp " +") ""
let digit_val ch = Char.code ch - 48
