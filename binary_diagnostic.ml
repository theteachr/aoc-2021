open Base
open Core

type input = int list list
type output = int

let string_of_output = Int.to_string

let ( << ) = Fn.compose

let read path =
  In_channel.read_lines path
  |> List.(map ~f:(map ~f:Char.get_digit_exn << String.to_list))

let bin_to_dec =
  let open List in
  foldi ~init:0 ~f:(fun i acc bit -> acc + (Base.( ** ) 2 i) * bit)

let negate x =
  if x <> 0 then 0 else 1

let solve_two (_ : input) : output = 0

let solve_one data =
  let open List in
  let counts = fold_left data
    ~init:(init (length @@ hd_exn data) ~f:(fun _ -> (0, 0)))
    ~f:(fun acc row -> zip_exn acc row
      |> map ~f:(fun ((zeros, ones), digit) ->
          let (zu, ou) = if digit = 0 then (1, 0) else (0, 1) in
          (zeros + zu, ones + ou))) in
  let g = rev_map counts ~f:(fun (z, o) -> if z > o then 0 else 1) in
  let e = map g ~f:negate in
  map [g; e] ~f:bin_to_dec
      |> fold_left ~init:1 ~f:( * )
