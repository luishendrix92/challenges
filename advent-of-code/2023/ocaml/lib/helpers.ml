(** Backlogged utility functions yet to be categorized.

    If there are no more than 5 functions of a category or if there is no suitable
    one to fit such functions, then they will stay inside this module. *)

(** [read_input] reads the contents of a challenge day input txt file into
    a list of string lines that can be fed into parsing functions. If the
    optional argument [sample] is set to [true], the input file name
    changes {b environment} from ["dayNN-full.txt"] to ["dayNN-sample.txt"]- *)
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

(** [int_exp] is the integer equivalent of [( ** )] float exponentiation but
    this version doesn't break when the exponent is negative, it floors the
    fractional result to 0. If this is not the intended behaviour, please
    ocnsider using Jane Street's [Core.Int.( ** )] instead. *)
let int_exp x n = int_of_float (float_of_int x ** float_of_int n)