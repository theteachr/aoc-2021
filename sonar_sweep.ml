open Base

type input = int list
type output = int

let string_of_output = Int.to_string 

let read path =
  Core.In_channel.read_lines path
  |> List.map ~f:Int.of_string

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

let _test_input = [199; 200; 208; 210; 200; 207; 240; 269; 260; 263]

let _test_one = solve_one _test_input
let _test_two = solve_two _test_input

let _input = read "inputs/01.txt"

let _answer_one = solve_one (_input)
let _answer_two = solve_two (_input)
