type direction = Forward | Down | Up

type input = (direction * int) list
type output = int

let string_of_output = string_of_int

let ( << ) f g x = x |> g |> f

let read file_content =
  let pair = function
    | [direction; value] -> begin
      let d = match direction with
      | "forward" -> Forward
      | "down" -> Down
      | "up" -> Up
      | _ -> failwith "Invalid direction"
      in (d, int_of_string value)
    end
    | _ -> failwith "Invalid number of items in the list"
  in
  String.split_on_char '\n' file_content
  |> List.map (pair << (String.split_on_char ' '))

let solve_one input =
  let (x, y) = List.fold_left (fun (x, y) (direction, value) ->
    match direction with
    | Forward -> (x + value, y)
    | Down -> (x, y + value)
    | Up -> (x, y - value)
  ) (0, 0) input
  in
  x * y

let solve_two input =
  let (x, y, _) = List.fold_left (fun (x, y, z) (direction, value) ->
    match direction with
    | Forward -> (x + value, y + z * value, z)
    | Down -> (x, y, z + value)
    | Up -> (x, y, z - value)
  ) (0, 0, 0) input
  in
  x * y
