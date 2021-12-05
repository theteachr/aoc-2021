open Base
open Core

type input = (string * int) list
type output = int

let string_of_output = Int.to_string

let ( << ) = Fn.compose

(* XXX: a type for direction (results in a less number of `failwith`s) *)
let read path =
  let pair = function
    | [direction; value] -> (direction, Int.of_string value)
    | _ -> failwith "unreachable"
  in
  In_channel.read_lines path
  |> List.map ~f:(pair << (String.split ~on:' '))

let solve_one input =
  let (x, y) = List.fold input ~init:(0, 0) ~f:(fun (x, y) (direction, value) ->
    match direction with
    | "forward" -> (x + value, y)
    | "down" -> (x, y + value)
    | "up" -> (x, y - value)
    | _ -> failwith "unreachable"
  )
  in
  x * y

let solve_two input =
  let (x, y, _) = List.fold input ~init:(0, 0, 0) ~f:(fun (x, y, z) (direction, value) ->
    match direction with
    | "forward" -> (x + value, y + z * value, z)
    | "down" -> (x, y, z + value)
    | "up" -> (x, y, z - value)
    | _ -> failwith "unreachable"
  )
  in
  x * y
