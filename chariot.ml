(* XXX: different types for part one and two *)
(* TODO: command line arguments
 - run all
 - run a specific part 
 - run both parts of a day
 - time *)

let ( << ) f g x = x |> g |> f

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
(*
  (module Binary_diagnostic);
  (module Squid);
*)
|]

let read_to_string filepath =
  let channel = open_in filepath in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  String.trim content

let print_solutions i (module M : Solver) =
  let open M in
  let open Printf in
  let open Stdio in

  print_endline (sprintf "Day %02d" (i + 1)); (* FIXME *)

  let input_data = read_to_string (sprintf "inputs/%02d.txt" (i + 1)) |> read in
  List.map (( |> ) input_data) [solve_one; solve_two]
  |> List.iteri (fun j solution ->
      print_endline (sprintf "  Part %02d: %s" (j + 1) (string_of_output solution)))

let () = Array.iteri print_solutions solvers
