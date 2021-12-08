type input = (string * int) list
type output = int

let string_of_output = string_of_int

let ( << ) f g x = x |> g |> f

(* XXX: a type for direction (results in a less number of `failwith`s) *)
let read file_content =
  let pair = function
    | [direction; value] -> (direction, int_of_string value)
    | _ -> failwith "Unreachable"
  in
  String.split_on_char '\n' file_content
  |> List.map (pair << (String.split_on_char ' '))

let solve_one input =
  let (x, y) = List.fold_left (fun (x, y) (direction, value) ->
    match direction with
    | "forward" -> (x + value, y)
    | "down" -> (x, y + value)
    | "up" -> (x, y - value)
    | _ -> failwith "unreachable"
  ) (0, 0) input
  in
  x * y

let solve_two input =
  let (x, y, _) = List.fold_left (fun (x, y, z) (direction, value) ->
    match direction with
    | "forward" -> (x + value, y + z * value, z)
    | "down" -> (x, y, z + value)
    | "up" -> (x, y, z - value)
    | _ -> failwith "unreachable"
  ) (0, 0, 0) input
  in
  x * y
