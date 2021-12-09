type input = int array array
type output = int

let string_of_output = string_of_int

let ( << ) f g x = x |> g |> f

let read =
  let digit_of_char c = int_of_char c - 48 in
  let list_of_string = List.of_seq << String.to_seq in
  Array.of_list <<
  List.(map (Array.of_list << map digit_of_char << list_of_string)) <<
  String.split_on_char '\n'

let solve_one data = Array.length data
let solve_two data = Array.length data
