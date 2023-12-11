open Advent.Parsing
open Advent.Helpers

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
  match read_input __FILE__ with
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

let part_3 () =
  let duration = 55826490
  and record_distance = 246144110121111 in
  let ways : int ref = ref 0 in
  for h = 0 to duration do
    if h * (duration - h) > record_distance then ways := !ways + 1
  done;
  Printf.printf "Solution Part 2: %d\n" !ways
;;

let () =
  part_1 ();
  (* part_2 () *)
  part_3 ()
;;

(* Musings and Scribbles - Nothing to see here... unless ;)
   ========================================================

   h -> button hold time (x axis | func input)
   v -> 1-to-1 mapping of h (implicit)
   d -> distance traveled (y axis | func output)
   t -> remaining time (derivative y axis)
   T -> available time for race (constant)
   D -> record distance to beat (constant)

   For T = 7 , D = 9
   -----------------------------------
   h = 0, t = 7, v = 0, d =  0
   h = 1, t = 6, v = 1, d =  6
   h = 2, t = 5, v = 2, d = 10 <-
   h = 3, t = 4, v = 3, d = 12 <- Four
   h = 4, t = 3, v = 4, d = 12 <- Ways
   h = 5, t = 2, v = 5, d = 10 <-
   h = 6, t = 1, v = 6, d =  6
   h = 7, t = 0, v = 7, d =  0

   d
   ▲             _
   12 ┤           ●   ●
   11 ┤         .       .
   10 ┤       ●           ●
   9 ┤------+-------------+----------
   8 ┤     .|             |.
   7 ┤    . |             | .
   6 ┤   ●  |             |  ●
   5 ┤  .   |             |   .
   4 ┤ .    |             |    .
   3 ┤.     |             |     .
   2 ┤      |             |      .
   1 ┤      |             |      |
   ───●───┬──+┬───┬───┬───┬+──┬───●─▸h
   0 │   1   2   3   4   5   6   7

   (x1, x2) = Intersection roots of
   - f(h) = h * (T - h) or
     f(h) = Th - h^2 + Th - h^2 or
     f(h) = -2h^2 + 2Th or simplified
     f(h) = -h^2 + Th => -h^2 + 7h
   - y = 9 (record to beat)

   Notice that since the segment is horizontal, I can
   just add a -D (in this case, -9) to the end of
   equation, then solve with the quadratic formula.

   Ways = abs(floor(x_2) - ceil(x_1)) - offset
   Where offset is determined by checking if the roots
   happen to be integer numbers (xn % xn = 0).

   Also a good observation is that, the derivative of f(h)
   outputs the time left in the race after holding the
   button for h time. The root (3 something ms) corresponds
   to the time it takes to reach maximum distance before
   being unable to complete the race or running out of time.
*)
