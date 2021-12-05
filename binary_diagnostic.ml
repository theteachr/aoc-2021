open Base
open Core

let string_of_output = Int.to_string

let ( << ) = Fn.compose
let ( +~ ) n b = n + Bool.to_int b

let read path =
  In_channel.read_lines path
  |> List.(map ~f:(Array.map ~f:Char.get_digit_exn << String.to_array))
  |> Array.of_list

let rates data =
  let open Array in
  let aux acc row =
    zip_exn acc row
    |> map ~f:(fun ((zeros, ones), digit) ->
        let update = digit = 0 in
        (zeros +~ update, ones +~ (not update)))
  in
  fold data
    ~init:(init (length @@ data.(0)) ~f:(fun _ -> (0, 0)))
    ~f:aux

let bin_arr_to_dec arr =
  let lst = Array.to_list arr |> List.rev in
  List.foldi lst
    ~init:0
    ~f:(fun i acc bit -> acc + (Base.( ** ) 2 i) * Bool.to_int bit)

let solve_one data =
  let (g, e) =
    rates data
    |> Array.map ~f:(fun (z, o) -> (z < o, z >= o))
    |> Array.unzip
  in
  bin_arr_to_dec g * bin_arr_to_dec e

let solve_two data =
  let _x = Array.fold_until data
  ~init:(rates data, data, 0)
  ~f:(fun (ratings, rest, i) _ ->
    if i = 5 (*Array.length rest = 1*) then Stop (rest, 800)
    else let filtered = Array.filter rest ~f:(fun arr ->
      let (z, o) = ratings.(i) in
      let bit = match (Int.compare z o) with
      | -1 | 0 -> 0
      | _ -> 1
      in
      arr.(i) <> bit) in Continue (rates filtered, filtered, i + 1))
  ~finish:(fun _ -> (data, 100))
  in
  _x


let input = read "inputs/03.txt"
let ans_one = solve_one input
let ans = solve_two input
let r = rates input
