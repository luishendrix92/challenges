open Advent.Parsing
open Advent.Helpers

(** [lagrange_interpolation] interpolates a set of historic values [f(x)] where
    [x] is equal to the value's index (starting from 0). Direct imperative
    implementation of Lagrange's Interpolating Polynomial as extracted from
    https://mathworld.wolfram.com/LagrangeInterpolatingPolynomial.html *)
let lagrange_interpolation history x =
  let n = Array.length history in
  let p : float ref = ref 0. in
  for k = 0 to n - 1 do
    let l : float ref = ref history.(k) in
    for i = 0 to n - 1 do
      if not (i = k)
      then (
        let term =
          (x -. float_of_int i) /. (float_of_int k -. float_of_int i)
        in
        l := !l *. term)
    done;
    p := !p +. !l
  done;
  !p
;;

let parsed_history_list =
  read_input ~sample:false __FILE__
  |> List.map (Array.of_list @@ floats_of_string)
;;

let extrapolation_sum step =
  let polynoms = parsed_history_list |> List.map lagrange_interpolation in
  polynoms
  |> List.map (fun polynomial -> polynomial step)
  |> List.fold_left ( +. ) 0.
  |> Float.ceil
;;

let part_1 =
  let next_index = List.hd parsed_history_list |> Array.length in
  extrapolation_sum (float_of_int next_index)
;;

let part_2 = extrapolation_sum (-1.)

let () =
  (* 1861775706 *)
  Printf.printf "Part 1 Solution: %d\n" (int_of_float part_1);
  (* 1082 *)
  Printf.printf "Part 2 Solution: %d\n" (int_of_float part_2)
;;
