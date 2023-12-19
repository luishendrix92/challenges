open Advent.Parsing
open Advent.Helpers
open Advent.Containers

let parsed_pipe_map = read_input ~sample:false __FILE__ |> lines_to_matrix

let direction_change ~current_dir pipe_type =
  match pipe_type, current_dir with
  | 'F', (-1, 0) | 'L', (1, 0) -> 0, 1
  | 'F', (0, -1) | '7', (0, 1) -> 1, 0
  | '7', (-1, 0) | 'J', (1, 0) -> 0, -1
  | 'J', (0, 1) | 'L', (0, -1) -> -1, 0
  | '-', dir | '|', dir -> dir
  | _ -> raise (Failure "Invalid entry point.")
;;

type entity =
  { mutable y : int
  ; mutable x : int
  ; mutable steps : int
  ; direction : int * int
  }

let move_rat rat =
  let deltaY, deltaX = rat.direction in
  rat.y <- rat.y + deltaY;
  rat.x <- rat.x + deltaX;
  rat.steps <- rat.steps + 1
;;

let traced_loop = Array.make_matrix 140 140 '.'

(* NOTE: Lines {31,39,42} help solve part 2 by tracing the main loop. *)
let steps_to_farthest ~rat_pos:(y, x) pipe_map =
  let rec traverse_loop rat =
    move_rat rat;
    match get2D pipe_map ~row:rat.y ~col:rat.x with
    | Some 'S' ->
      traced_loop.(rat.y).(rat.x) <- 'F';
      rat.steps
    | Some pipe ->
      traced_loop.(rat.y).(rat.x) <- pipe;
      let next_dir = direction_change ~current_dir:rat.direction pipe in
      traverse_loop { rat with direction = next_dir }
    | None -> raise (Failure "Position out of map.")
  in
  (* HACK: The challenge requirements facilitate initial dir hardcoding. *)
  traverse_loop { y; x; steps = 0; direction = 0, 1 }
;;

let part_1 () =
  match pos_in_matrix ~f:(fun ch -> ch = 'S') parsed_pipe_map with
  | Some rat_pos ->
    let loop_steps = parsed_pipe_map |> steps_to_farthest ~rat_pos in
    Printf.printf "Part 1 Solution: %d\n" (loop_steps / 2)
  | None -> raise (Failure "No rat (S) found inside the input file.")
;;

let ray_casting ~charset line ch_idx =
  let count : int ref = ref 0 in
  for i = 0 to ch_idx - 1 do
    if CharSet.mem line.(i) charset then Core.Int.incr count
  done;
  is_odd !count
;;

let part_2 () =
  let is_contained = ray_casting ~charset:(CharSet.of_list [ 'L'; 'J'; '|' ]) in
  let contained_count =
    Core.Array.fold
      ~f:(fun sum line ->
        sum
        + Core.Array.foldi
            ~f:(fun i sum ch ->
              sum + if ch = '.' then is_contained line i |> Bool.to_int else 0)
            ~init:0
            line)
      ~init:0
      traced_loop
  in
  Printf.printf "Part 2 Solution: %d\n" contained_count
;;

let () =
  part_1 ();
  part_2 ()
;;
