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

let floats_of_string s = split_on " +" s |> List.map float_of_string
let ints_of_string s = split_on " +" s |> List.map int_of_string
let data_col = List.hd @@ List.tl @@ split_on ":"
let float_of_nums = float_of_string @@ Str.global_replace (Str.regexp " +") ""
let digit_val ch = Char.code ch - 48
