type input = int list list
type output = int

let string_of_output = string_of_int

let ( << ) f g x = x |> g |> f

let read =
  let digit_of_char c = int_of_char c - 48 in
  let list_of_string = List.of_seq << String.to_seq in
  List.(map (map digit_of_char << list_of_string)) <<
  String.split_on_char '\n'

let frequencies data =
  let open List in
  let bit_width = (length << hd) data in
  let init_counts = init bit_width (fun _ -> 0) in
  fold_left (map2 ( + )) init_counts data

let solve_one data =
  let dec_of_bool_array =
    let open List in
    int_of_float
    << snd
    << fold_left (fun (pow, value) bit -> pow +. 1., value +. 2. ** pow *. bit) (0., 0.)
    << map Bool.to_float
  in
  let half = List.length data / 2 in
  let gamma_rate =
    frequencies data
    |> List.rev_map (( <= ) half)
  in
  let epsilon_rate = List.map not gamma_rate in
  dec_of_bool_array gamma_rate * dec_of_bool_array epsilon_rate

let solve_two data = List.length data
