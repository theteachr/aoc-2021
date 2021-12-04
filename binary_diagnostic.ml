open Base
open Core

type input = int array array
type output = int

let string_of_output = Int.to_string

let ( << ) = Fn.compose
let ( +~ ) n b = n + Bool.to_int b

let read path =
  In_channel.read_lines path
  |> List.(map ~f:(Array.map ~f:Char.get_digit_exn << String.to_array))
  |> Array.of_list

let rates data =
  let open Array in
  let counts = fold data
    ~init:(init (length @@ data.(0)) ~f:(fun _ -> (0, 0)))
    ~f:(fun acc row -> zip_exn acc row
      |> map ~f:(fun ((zeros, ones), digit) ->
          let update = digit = 0 in
          (zeros +~ update, ones +~ (not update))))
  in
  map counts ~f:(fun (z, o) -> (z <= o, z > o)) |> unzip

let solve_one data =
  let bin_arr_to_dec arr =
    let lst = Array.to_list arr |> List.rev in
    List.foldi lst
    ~init:0
    ~f:(fun i acc bit -> acc + (Base.( ** ) 2 i) * Bool.to_int bit)
  in
  let (g, e) = rates data in
  bin_arr_to_dec g * bin_arr_to_dec e

let solve_two data =
  let open Array in
  let (g, e) = rates data in
  let _x = fold_until data
  ~init:(data, 0)
  ~f:(fun (rest, i) _ ->
    if length rest = 1 then Stop rest.(0)
    else Continue (filter rest ~f:(fun arr -> arr.(i) = Bool.to_int g.(i)), i + 1))
  ~finish:(fun _ -> map e ~f:Bool.to_int)
  in
  0


let input = read "inputs/03.txt"
let ans_one = solve_one input
let ans = solve_two input
