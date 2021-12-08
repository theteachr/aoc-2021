type input = int list
type output = int

let string_of_output = string_of_int

let ( << ) f g x = x |> g |> f

let read = List.map int_of_string << String.split_on_char '\n'

let solve_one report =
  let rec aux inc_count = function
    | [] -> inc_count
    | hd :: (hd' :: _ as tl) when hd < hd' -> aux (inc_count + 1) tl
    | _ :: tl -> aux inc_count tl
  in
  aux 0 report

let solve_two report =
  let rec aux inc_count = function
    | [] -> inc_count
    | a :: (_ :: _ :: b :: _ as tl)
    when a < b -> aux (inc_count + 1) tl
    | _ :: tl -> aux inc_count tl
  in
  aux 0 report
