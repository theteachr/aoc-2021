open Base
open Core

type input = int list list
type output = int

let string_of_output = Int.to_string

let ( << ) = Fn.compose
let ( +~ ) n b = n + Bool.to_int b

let read path =
  In_channel.read_lines path
  |> List.(map ~f:(map ~f:Char.get_digit_exn << String.to_list))

let bin_to_dec =
  let open List in
  foldi ~init:0 ~f:(fun i acc bit -> acc + (Base.( ** ) 2 i) * Bool.to_int bit)

let sign_bit_mask = lnot (1 lsl 62)

let solve_two (_ : input) : output = 0

let solve_one data =
  let open List in
  let counts = fold_left data
    ~init:(init (length @@ hd_exn data) ~f:(fun _ -> (0, 0)))
    ~f:(fun acc row -> zip_exn acc row
      |> map ~f:(fun ((zeros, ones), digit) ->
          let update = digit = 0 in
          (zeros +~ update, ones +~ (not update)))) in
  let g, e = rev_map counts ~f:(fun (z, o) -> (z > o, z <= o)) |> unzip in
  bin_to_dec g * bin_to_dec e

let ans = solve_one (read "inputs/03.txt")
