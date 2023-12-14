(** Backlogged utility functions yet to be categorized.

    If there are no more than 5 functions of a category or if there is no suitable
    one to fit such functions, then they will stay inside this module. *)

(** [read_input] reads the contents of a challenge day input txt file into
    a list of string lines that can be fed into parsing functions. *)
let read_input ?(sample = false) callee_filename =
  let open Filename in
  let input_filename =
    "inputs"
    ^ dir_sep
    ^ basename (remove_extension callee_filename)
    ^ (if sample then "-sample" else "-full")
    ^ ".txt"
  in
  Core.In_channel.read_lines input_filename
;;