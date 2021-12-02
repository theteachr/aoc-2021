open Base

(* TODO: different types for part one and two *)

module type Solver = sig
  type input
  type output

  val read : string -> input
  val solve_one : input -> output
  val solve_two : input -> output
  val string_of_output : output -> string
end

let solvers : (module Solver) array = [|
  (module Sonar_sweep);
  (module Dive);
|]

let () = Array.iteri solvers ~f:(fun i (module M) ->
  let open M in
  solve_one @@ read (Printf.sprintf "inputs/%02d.txt" (i + 1))
  |> M.string_of_output
  |> Stdio.print_endline)
