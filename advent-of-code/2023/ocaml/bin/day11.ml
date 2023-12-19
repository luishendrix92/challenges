open Advent.Parsing
open Advent.Helpers
open Advent.Containers

let expand universe =
  let expansion expanded galaxy_row =
    Array.append
      expanded
      (if Array.for_all (fun ch -> ch = '.') galaxy_row
       then Array.make 2 galaxy_row
       else [| galaxy_row |])
  in
  Array.fold_left expansion [||] universe
  |> Core.Array.transpose_exn
  |> Array.fold_left expansion [||]
  |> Core.Array.transpose_exn
;;

let parsed_universe = read_input ~sample:false __FILE__ |> lines_to_matrix
let expanded_universe = expand parsed_universe

let size_of_matrix matrix =
  let rows = Array.length matrix in
  if rows <> 0 then rows, Array.length matrix.(0) else 0, 0
;;

let manhattan_distance (y1, x1) (y2, x2) = abs (x1 - x2) + abs (y1 - y2)

(* TODO: Abstract the nested loop into a matrix_foldi helper. *)
let part_1 =
  let rows, cols = size_of_matrix expanded_universe in
  let galaxies : 'a list ref = ref [] in
  for y = 0 to rows - 1 do
    for x = 0 to cols - 1 do
      if expanded_universe.(y).(x) = '#' then galaxies := (y, x) :: !galaxies
    done
  done;
  unique_pairs !galaxies
  |> List.map (fun (g1, g2) -> manhattan_distance g1 g2)
  |> List.fold_left ( + ) 0
;;

let () = Printf.printf "Part 1 Solution: %d\n" part_1
