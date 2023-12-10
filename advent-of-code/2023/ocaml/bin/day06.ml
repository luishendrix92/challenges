open Advent.Parsing

let parable_equation (available_time, record_distance) =
  -1., available_time, -.record_distance
;;

(* TODO: Refactor mathematically to avoid DRY code in this function. *)
let quadratic_formula (a, b, c) =
  let x1 = (-.b -. sqrt ((b ** 2.) -. (4. *. a *. c))) /. (2. *. a) in
  let x2 = (-.b +. sqrt ((b ** 2.) -. (4. *. a *. c))) /. (2. *. a) in
  x1, x2
;;

let boat_races, single_boat_race =
  match read_lines "./inputs/day06-full.txt" with
  | line1 :: line2 :: _ ->
    let times = data_col line1 in
    let distances = data_col line2 in
    ( Core.List.zip_exn (floats_of_string times) (floats_of_string distances)
    , (float_of_nums times, float_of_nums distances) )
  | _ -> raise (Failure "Input file doesn't have 2+ lines.")
;;

let ways_to_beat race =
  let max_hold, min_hold = race |> parable_equation |> quadratic_formula in
  let offset =
    Bool.to_float (Float.is_integer max_hold && Float.is_integer min_hold)
  in
  int_of_float (ceil max_hold -. ceil min_hold -. offset)
;;

let part_1 () =
  List.map ways_to_beat boat_races
  |> List.fold_left ( * ) 1
  |> Printf.printf "Part 1 Solution: %d\n"
;;

let part_2 () =
  ways_to_beat single_boat_race |> Printf.printf "Part 2 Solution: %d\n"
;;

let () =
  (* part_1 (); *)
  part_2 ()
;;
