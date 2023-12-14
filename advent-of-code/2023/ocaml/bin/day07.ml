open Advent.Containers
open Advent.Parsing
open Advent.Helpers
open Core.Comparable

module Hand = struct
  let card_strength = function
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | digit -> digit_val digit
  ;;

  let of_string s = Core.String.to_list s

  let compare_hands =
    let by_type_strength =
      lexicographic [ reverse List.compare_lengths; List.compare compare ]
    in
    let by_card_strength = List.compare (lift compare ~f:card_strength) in
    lexicographic
      [ lift by_type_strength ~f:sorted_frequencies; by_card_strength ]
  ;;
end

let parsed_data =
  let parse_line line =
    match split_on " " line with
    | hand :: bid :: _ -> Hand.of_string hand, int_of_string bid
    | _ -> raise (Failure "Not enough data columns to parse line (2)")
  in
  read_input ~sample:true __FILE__ |> List.map parse_line
;;

let part_1 =
  parsed_data
  |> List.stable_sort (lift Hand.compare_hands ~f:fst)
  |> List.mapi (fun i (_, bid) -> (i + 1) * bid)
  |> List.fold_left ( + ) 0
;;

(** TODO: Implement *)
let part_2 = 0

let () =
  Printf.printf "Solution To Part 1:  %d\n" part_1;
  Printf.printf "Solution To Part 2: %d\n" part_2
;;