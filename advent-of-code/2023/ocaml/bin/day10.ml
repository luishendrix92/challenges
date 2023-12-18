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

(* NOTE: Lines {33,38,41} help solve part 2 by tracing the main loop. *)
let steps_to_farthest ~rat_pos:(y, x) pipe_map =
  let traced_loop = Array.make_matrix 140 140 '.' in
  let rec traverse_loop rat =
    move_rat rat;
    match get2D pipe_map ~row:rat.y ~col:rat.x with
    | Some 'S' ->
      write_ch_matrix traced_loop ~fname:"./outputs/day10.txt";
      rat.steps
    | Some pipe ->
      traced_loop.(rat.y).(rat.x) <- pipe;
      let next_dir = direction_change ~current_dir:rat.direction pipe in
      traverse_loop { rat with direction = next_dir }
    | None -> raise (Failure "Position out of map.")
  in
  (* HACK: The challenge requirements facilitate the initial dir hardcoding. *)
  traverse_loop { y; x; steps = 0; direction = 0, 1 }
;;

let part_1 () =
  match pos_in_matrix ~f:(fun ch -> ch = 'S') parsed_pipe_map with
  | Some rat_pos ->
    let loop_steps = parsed_pipe_map |> steps_to_farthest ~rat_pos in
    Printf.printf "Part 1 Solution: %d\n" (loop_steps / 2)
  | None -> raise (Failure "No rat (S) found inside the input file.")
;;

let () = part_1 ()
