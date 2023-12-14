open Advent.Parsing
open Advent.Helpers

let almanac = read_input __FILE__ |> group_blocks

type conversion_map =
  { name : string
  ; ranges : (int * int * int) list
  }

let seeds =
  List.hd almanac
  |> List.hd
  |> split_on ":"
  |> List.tl
  |> List.hd
  |> ints_of_string
;;

let conversion_maps =
  let parse_range line =
    match ints_of_string line with
    | destination :: source :: range :: _ -> source, destination, range
    | _ -> raise (Failure "Badly formatted map range.")
  in
  let parse_map = function
    | name :: ranges -> { name; ranges = List.map parse_range ranges }
    | [] -> raise (Failure "Badly formatted map.")
  in
  List.tl almanac |> List.map parse_map
;;

let pipeline, pipeline_inverted =
  let conversion ?(inverted = false) ranges num =
    let map_value (source, destination, range) =
      let source, destination =
        if inverted then destination, source else source, destination
      in
      let a, b = source, source + range - 1 in
      let offset = num - source in
      if num >= a && num <= b then Some (destination + offset) else None
    in
    List.find_map map_value ranges |> Option.value ~default:num
  in
  ( conversion_maps |> List.map (fun map -> conversion map.ranges)
  , conversion_maps
    |> List.map (fun map -> conversion ~inverted:true map.ranges) )
;;

let seed_to_location seed_number =
  List.fold_left (fun x f -> f x) seed_number pipeline
;;

let min_location seeds =
  List.map seed_to_location seeds
  |> Core.List.min_elt ~compare
  |> Core.Option.value_exn
;;

let part_1 () = min_location seeds |> Printf.printf "Part 1 Solution: %d\n"

let seed_ranges =
  Core.List.groupi ~break:(fun i _ _ -> i mod 2 = 0) seeds
  |> List.map (function
    | start :: len :: _ -> start, start + len - 1
    | _ -> raise (Failure "Badly formatted seed range."))
;;

(* Optimization Attempt #1 - Parallel Camel :>).

   I could not for the love of me find a more optimal algoritham so it was time
   for some brute-forcing (processor go brr)! The first try was slow even with
   a million seed checks because every call to seed_to_location was re-creating
   the [pipeline] due to being inside the function's body as a let-in binding.
   After I moved [pipeline] outside I also decided to write [min_of_range]
   imperatively just for extra measure. I tested the code with 500M seeds and
   I remember it taking around [40 to 50 minutes]... Mind you, my laptop is 13
   years old with a Core i7-2620M, 4 gigs of RAM, and 4 cores @ 2.7GHz.

   Enter parallelism! Even with 4 cores I feel like I would rather wait like
   30 to 60 minutes than 2 or more hours. I did some googlin' and found that I
   could use the [Domain] primitive to spawn 1 sub-process (?) for each of my
   ranges and hope for the best. After letting it run overnight with Hyperfine
   as the benchmarking tool the result was: [21 minutes] average! Not bad. 

   let part_2 () =
     let min_of_range (a, b) =
       let result : int ref = ref Int.max_int in
       for i = a to b do
         result := min (seed_to_location i) !result
       done;
       !result
     in
     seed_ranges
     |> List.map (fun range -> Domain.spawn (fun _ -> min_of_range range))
     |> List.map Domain.join
     |> Core.List.min_elt ~compare
     |> Core.Option.value_exn
     |> Printf.printf "Part 2 Solution: %d\n"
   ;;
*)

(* Optimization Attempt #2 - Inverse parallel pipeline

   After a few hours of wondering how I could optimize this without
   parallelism, I realized that the answer to the solution was relatively low
   (79~M) so I thought I'd just start from the bottom location and work my way
   up checking if the location mapped to a seed (thus the inverted pipeline)
   that was within one of the provided seed ranges. Once the loop finds a
   location that satisfied the predicate, the program exits with the right
   answer. To invert the pipeline all I had to do was:

   1. Create an extra list of mapping functions but with the source and
   destination swapped. For example, turn seed-to-soil into soil-to-seed.
   2. Apply the pipeline value [x] but from right to left [fold_right].

   So anyway here are the hyperfine benchmarks, with a time of around [1m16s]!
   Time (mean ± σ):     76.789 s ±  1.910 s    [User: 76.785 s, System: 0.004 s]
   Range (min … max):   74.781 s … 81.118 s    10 runs
   I thought of parallelizing chunks of 10M locations but meh, I had enough...
*)

let location_to_seed loc_number =
  List.fold_right (fun f x -> f x) pipeline_inverted loc_number
;;

let part_2 () =
  let location_in_range location =
    let seed = location_to_seed location in
    List.exists
      (fun (seed_a, seed_b) -> seed >= seed_a && seed <= seed_b)
      seed_ranges
  in
  for i = 0 to Int.max_int do
    if location_in_range i
    then (
      Printf.printf "Part 2 Solution: %d\n" i;
      exit 0)
  done
;;

let () =
  part_1 ();
  part_2 ()
;;
