type input = int list list
type output = int

let string_of_output = Int.to_string

let ( << ) f g x = x |> g |> f

let list_of_string = List.of_seq << String.to_seq

let read =
  let digit_of_char c = int_of_char c - 48 in
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
  << rev

let flip_bool_list = List.map not

(** Returns a list of booleans representing
    the gamma rate. *)
let gamma_rate_bools data =
  let half = List.length data / 2 in
  frequencies data |> List.map (( <= ) half)

let solve_one data =
  let gr = gamma_rate_bools data in
  let er = List.map not gr in
  List.(fold_left ( * ) 1 (map dec_of_bool_list [gr; er]))

let bool_list_of_string = List.map (( = ) '1') << list_of_string

let solve_two data = List.length data

let data = read "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
let answer = solve_one data
