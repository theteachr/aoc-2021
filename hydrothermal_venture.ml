open Base
open Core

let ( << ) = Fn.compose

module Point = struct

  type t = { x : int; y : int }

  let create x y = { x; y }

  let of_list = function
    | [a; b] -> { x = a; y = b }
    | _ -> failwith "Inapproiate list"

  let range a b =
    let op = if a <= b then ( + ) else ( - ) in
    List.init ((abs (a - b)) + 1) (op a)

  let between ({ x = x1; y = y1 }, { x = x2; y = y2 }) =
    let xs = range x1 x2 in
    let ys = range y1 y2 in

    let open List.Monad_infix in
    xs >>= fun x -> List.map ys ~f:(create x)

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

let solve_one =
  let open List in
  map ~f:Point.between
  << filter ~f:(fun ((p1, p2) : (Point.t * Point.t)) -> p1.x = p2.x || p1.y = p2.y)

let data = read "inputs/05.txt"
let answer = solve_one data
