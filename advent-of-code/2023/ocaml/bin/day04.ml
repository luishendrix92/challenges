open Advent.Parsing
open Advent.Helpers
module IntSet = Set.Make (Int)

(** A card is represented by two sets of numbers: first,
    the winning numbers, and second, my numbers. *)
let parse_card card_line =
  let parse_set str_nums = ints_of_string str_nums |> IntSet.of_list in
  let sets =
    data_col card_line |> String.split_on_char '|' |> List.map parse_set
  in
  match sets with
  | winning :: own :: _ -> winning, own
  | _ -> raise (Failure "Parse failed, not enough sets (2).")
;;

(** Computes a card's points by intersecting both number sets and using its
    count [n] to duplicate [1] point [n - 1] times. Returns a tuple with the
    card's points and the intersection count (useful for part 2). *)
let card_points (winning, own) =
  let count = IntSet.inter winning own |> IntSet.cardinal in
  int_exp 2 (count - 1), count
;;

let scored_cards =
  read_input __FILE__
  |> List.map (fun line -> card_points (parse_card line))
  |> Array.of_list
;;

let part_1 () =
  scored_cards
  |> Array.fold_left (fun sum (points, _) -> sum + points) 0
  |> Printf.printf "Part 1 Solution: %d\n"
;;

(** Forgive me father for I have sined. This function is imperative as hell
    but still succeeds in counting won copies recursively (cache optimized)
    until there are no more ([n = 0]) won copies to check. *)
let count_copies cards =
  let len = Array.length cards in
  let cache = Hashtbl.create len in
  let rec aux idx n =
    let count : int ref = ref 0 in
    for i = idx to min (idx + n) len - 1 do
      let copies_won =
        match Hashtbl.mem cache i, cards.(i) with
        | _, 0 -> 0 (* Didn't win any copies *)
        | true, _ -> Hashtbl.find cache i (* Cache hit *)
        | _, n ->
          (* Cache Miss *)
          let copies = aux (i + 1) n in
          Hashtbl.add cache i copies;
          copies
      in
      count := !count + 1 + copies_won
    done;
    !count
  in
  aux 0 len
;;

let part_2 () =
  Array.map snd scored_cards
  |> count_copies
  |> Printf.printf "Part 2 Solution: %d\n"
;;

let () =
  part_1 ();
  part_2 ()
;;
