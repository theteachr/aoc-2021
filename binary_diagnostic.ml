type input = int list list
type output = int

let string_of_output = Int.to_string

let ( << ) f g x = x |> g |> f

let read =
  let digit_of_char c = int_of_char c - 48 in
  let list_of_string = List.of_seq << String.to_seq in
  List.(map (map digit_of_char << list_of_string))
  << String.split_on_char '\n'

let frequencies data =
  let open List in
  let bit_width = (length << hd) data in
  let init_counts = init bit_width (fun _ -> 0) in
  fold_left (map2 ( + )) init_counts data

let dec_of_bool_list =
  let open List in
  Int.of_float
  << snd
  << fold_left (fun (p, v) bit -> (p +. 1., v +. 2. ** p *. bit)) (0., 0.)
  << map Bool.to_float

let flip_bool_list = List.map not

let gamma_rate data =
  let half = List.length data / 2 in
  frequencies data |> List.rev_map (( <= ) half)

let epsilon_rate = flip_bool_list << gamma_rate

let solve_one data =
  dec_of_bool_list (gamma_rate data) * dec_of_bool_list (epsilon_rate data)

let solve_two data = List.length data
