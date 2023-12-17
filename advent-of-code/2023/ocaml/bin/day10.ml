open Advent.Parsing
open Advent.Helpers
open Advent.Containers

type matrix = char array array

let parsed_pipe_map : matrix =
  read_input ~sample:false __FILE__ |> lines_to_matrix
;;

let direction_change ~current_dir pipe_type =
  match pipe_type, current_dir with
  | 'F', (-1, 0) | 'L', (1, 0) -> Some (0, 1)
  | 'F', (0, -1) | '7', (0, 1) -> Some (1, 0)
  | '7', (-1, 0) | 'J', (1, 0) -> Some (0, -1)
  | 'J', (0, 1) | 'L', (0, -1) -> Some (-1, 0)
  | '-', dir | '|', dir -> Some dir
  | _ -> None
;;

type entity =
  { mutable y : int
  ; mutable x : int
  ; mutable steps : int
  ; direction : (int * int) option
  }

let move_rat rat =
  match rat.direction with
  | Some (deltaY, deltaX) ->
    rat.y <- rat.y + deltaY;
    rat.x <- rat.x + deltaX;
    rat.steps <- rat.steps + 1
    (* Printf.printf "Rat: (%d, %d) | Steps: %d\n" rat.y rat.x rat.steps *)
    (* Unix.sleepf 0.3 *)
  | None -> ()
;;

let steps_to_farthest ~rat_pos:(y, x) (pipe_map : matrix) =
  let open Core.Option in
  let rec traverse_loop rat =
    move_rat rat;
    match rat.direction, get2D pipe_map ~row:rat.y ~col:rat.x with
    | Some _, Some 'S' -> rat.steps
    | Some current_dir, next_char ->
      let next_dir = next_char >>= direction_change ~current_dir in
      traverse_loop { rat with direction = next_dir }
    | None, _ ->
      let valid_dir = 0, 1 in
      traverse_loop { rat with direction = Some valid_dir }
  in
  traverse_loop { y; x; steps = 0; direction = None }
;;

let part_1 () =
  match pos_in_matrix ~f:(fun ch -> ch = 'S') parsed_pipe_map with
  | Some rat_pos ->
    let loop_steps = parsed_pipe_map |> steps_to_farthest ~rat_pos in
    Printf.printf "Part 1 Solution: %d\n" (loop_steps / 2)
  | None -> raise (Failure "No rat (S) found inside the input file.")
;;

let () = part_1 ()
