type input = int list
type output = int

let string_of_output = Int.to_string

let ( << ) f g x = x |> g |> f

let read = List.map int_of_string << String.split_on_char '\n'

let solve_one report =
  let rec aux inc_count = function
    | [] -> inc_count
    | x :: (x' :: _ as xs) when x < x' -> aux (inc_count + 1) xs
    | _ :: xs -> aux inc_count xs
  in
  aux 0 report

let solve_two report =
  let rec aux inc_count = function
    | [] -> inc_count
    | x :: (_ :: _ :: x' :: _ as xs) when x < x' -> aux (inc_count + 1) xs
    | _ :: xs -> aux inc_count xs
  in
  aux 0 report
