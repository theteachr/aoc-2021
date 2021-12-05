open Base
open Core

let string_of_output = Int.to_string

let ( << ) = Fn.compose

module Board = struct
  type t = int array array

  let row_of_line = List.map ~f:Int.of_string << Str.(split (regexp " +"))
  let of_lines = List.to_array << List.(map ~f:(to_array << row_of_line))
end

let read path =
  let drawer_line :: board_lines = In_channel.read_all path
  |> String.split_lines
  |> List.filter ~f:(not << String.is_empty)
  in
  let drawer = String.split drawer_line ~on:',' |> List.map ~f:Int.of_string in
  let boards = board_lines
  |> List.groupi ~break:(fun i _ _ -> i % 5 = 0)
  |> List.map ~f:Board.of_lines
  in
  (drawer, boards)

let (drawer, boards) = read "inputs/04.txt"
