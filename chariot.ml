open Base

(* XXX: different types for part one and two *)

let ( << ) = Fn.compose

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
  let open Printf in
  let open Stdio in

  print_endline (sprintf "Day %02d" (i + 1)); (* FIXME *)

  let input_data = read (sprintf "inputs/%02d.txt" (i + 1)) in

  List.map [solve_one; solve_two] ~f:(Fn.flip ( @@ ) input_data)
  |> List.iteri ~f:(fun j solution ->
      print_endline (sprintf "  Part %02d: %s" (j + 1) (string_of_output solution)
    )))
