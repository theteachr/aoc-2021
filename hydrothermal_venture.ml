open Base
open Core

let ( << ) = Fn.compose

module Point = struct

  type t = { x : int; y : int }

  let of_list = function
    | [a; b] -> { x = a; y = b }
    | _ -> failwith "Inapproiate list"

end

let read path =
  let pair = function
    | [a; b] -> (a, b)
    | _ -> failwith "Can't make a pair"
  in
  In_channel.read_lines path
  |> List.map ~f:(
    pair
    << List.map ~f:(
      Point.of_list
      << List.map ~f:Int.of_string
      << String.split ~on:',')
    << Str.(split (regexp " -> ")))


let answer = read "inputs/05.txt"
