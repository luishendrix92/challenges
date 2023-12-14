open Advent.Containers
open Advent.Parsing
open Advent.Helpers
open Core.Comparable

(* TODO: Rewrite in order to prevent too much ~joker argument passing. *)
module Hand = struct
  open Core.Int

  let card_strength ~joker = function
    | 'T' -> 10
    | 'J' -> if not joker then 11 else 1
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | digit -> digit_val digit
  ;;

  let of_string s = Core.String.to_list s

  let pair_counts ~joker hand =
    let counts = char_freq hand in
    if not joker
    then List.length counts, snd (List.hd counts)
    else begin
      let joker_count, without_jokers =
        ( List.assoc_opt 'J' counts |> Option.value ~default:0
        , List.remove_assoc 'J' counts )
      in
      match joker_count with
      | 5 -> 1, 5 (* Five of a kind (the joker) *)
      | count ->
        List.length without_jokers, snd (List.hd without_jokers) + count
    end
  ;;

  let compare_hands ~joker =
    let by_type_strength =
      lift
        (Core.Tuple2.compare ~cmp1:descending ~cmp2:ascending)
        ~f:(memo (pair_counts ~joker))
    in
    let by_card_strength =
      List.compare (lift ascending ~f:(card_strength ~joker))
    in
    lexicographic [ by_type_strength; by_card_strength ]
  ;;
end

let parsed_data =
  let parse_line line =
    match split_on " " line with
    | hand :: bid :: _ -> Hand.of_string hand, int_of_string bid
    | _ -> raise (Failure "Not enough data columns to parse line (2)")
  in
  read_input ~sample:false __FILE__ |> List.map parse_line
;;

let total_winnings ?(joker = false) hands =
  hands
  |> List.stable_sort (lift (Hand.compare_hands ~joker) ~f:fst)
  |> List.mapi (fun i (_, bid) -> (i + 1) * bid)
  |> List.fold_left ( + ) 0
;;

let part_1 = total_winnings parsed_data
let part_2 = total_winnings parsed_data ~joker:true

let () =
  (* 252295678 *)
  Printf.printf "Solution To Part 1: %d\n" part_1;
  (* 250577259 *)
  Printf.printf "Solution To Part 2: %d\n" part_2
;;
