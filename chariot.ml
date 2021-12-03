open Base

(* TODO: different types for part one and two *)

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
  List.iter [solve_one; solve_two] ~f:(
    Stdio.print_endline
    << string_of_output
    << Fn.flip ( @@ ) (read (sprintf "inputs/%02d.txt" (i + 1)))))
