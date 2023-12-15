(** Extended API to work with different container types.

    Jane Street's Core library, Stdlib and CC (an external library) are very
    complete when it comes to extending the API for container types such as
    lists, tuples, arrays, maps, sets, etc. But if I need specific operations
    that I haven't found in other libraries, then I'll put them here. *)

(** [group_list_by] returns a [StdLib.Hashtbl] where the keys are defined
    by the result of applying [f] to each value of a list [l], and their
    corresponding value will be a list of all the elements of [l].

    If the optional argument [initial_size] is not provided, the length of
    [l] will be used as the initial [Hashtbl] size instead. *)
let group_list_by ?initial_size f l =
  let size =
    match initial_size with
    | Some size -> size
    | None -> List.length l
  in
  List.fold_left
    (fun groups elt ->
      let group_key = f elt in
      (match Hashtbl.find_opt groups group_key with
       | Some group -> Hashtbl.replace groups group_key (elt :: group)
       | None -> Hashtbl.replace groups group_key [ elt ]);
      groups)
    (Hashtbl.create size)
    l
;;

module CharMap = Map.Make (Char)

(** [char_freq] takes a list of characters and returns an associative
    list of frequences sorted in descending order (by most frequent). *)
let char_freq l =
  let inc_count = function
    | Some count -> Some (count + 1)
    | None -> Some 1
  in
  List.fold_left
    (fun freqs ch -> CharMap.update ch inc_count freqs)
    CharMap.empty
    l
  |> CharMap.to_list
  |> List.sort (Core.Comparable.lift Core.Int.descending ~f:snd)
;;

(** [list_to_pairs] takes a list and returns a list of pairs which elements
    are the current [x_i] element and the next [x_(i+1)] thus having a final
    length of [n-1]. Analogous to (safely) zipping [l] with its own tail.

    {[
      let result = list_to_pairs [1;2;3;4] in
      assert (result = (1, 2); (2, 3); (3, 4)])
    ]}

    This implementation is non tail-recursive.*)
let rec list_to_pairs = function
  | x :: y :: rest -> (x, y) :: list_to_pairs (y :: rest)
  | _ -> []
;;
