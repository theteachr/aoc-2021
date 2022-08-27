open Base
open Core

type direction =
  | Forward
  | Down
  | Up

type input = (direction * int) list
type output = int

type parse_error =
  | InvalidDirection

let string_of_output = Int.to_string
let direction_of_string = function
  | "forward" -> Ok Forward
  | "down" -> Ok Down
  | "up" -> Ok Up
  | _ -> Error InvalidDirection


let ( << ) = Fn.compose

let read path =
  let pair = function
    | [direction; value] -> (direction_of_string direction, Int.of_string value)
    | _ -> failwith "Invalid line"
  in
  In_channel.read_lines path
  |> List.map ~f:(pair << (String.split ~on:' '))
  |> List.filter_map ~f:(fun (d, v) -> Result.ok d |> Option.map ~f:(fun d -> (d, v)))

let solve_one input =
  let (x, y) = List.fold input ~init:(0, 0) ~f:(fun (x, y) (direction, value) ->
    match direction with
    | Forward -> (x + value, y)
    | Down -> (x, y + value)
    | Up -> (x, y - value)
  )
  in
  x * y

let solve_two input =
  let (x, y, _) = List.fold input ~init:(0, 0, 0) ~f:(fun (x, y, z) (direction, value) ->
    match direction with
    | Forward -> (x + value, y + z * value, z)
    | Down -> (x, y, z + value)
    | Up -> (x, y, z - value)
  )
  in
  x * y
