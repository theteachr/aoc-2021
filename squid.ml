open Base
open Core

let string_of_output = Int.to_string

let ( << ) = Fn.compose

type cell_state = Marked of int | Unmarked of int

module Board = struct

  let row_of_line =
    List.map ~f:(fun s -> Unmarked (Int.of_string s)) << Str.(split (regexp " +"))

  let of_lines = List.to_array << List.(map ~f:(to_array << row_of_line))

  let mark board x =
    let fold_result =
      Array.fold_until board
      ~init:0
      ~f:(fun row_idx row ->
        let found = Array.findi row (fun _ n ->
          match n with
          | Unmarked y -> x = y
          | _ -> false)
        in
        match found with
        | Some (col_idx, _) ->  Stop (Some (row_idx, col_idx))
        | None -> Continue (row_idx + 1))
      ~finish:(fun _ -> None)
    in
    match fold_result with
    | Some (r, c) ->
        let Unmarked x = board.(r).(c) in
        board.(r).(c) <- Marked x
    | _ -> ()

  let all_marked =
    Array.for_all ~f:(fun c ->
      match c with
      | Marked _ -> true
      | _ -> false)

  let bingoed_row board =
    Array.fold_until board ~init:0 ~f:(fun row_idx row ->
      if all_marked row then Stop (Some row_idx)
      else Continue (row_idx + 1))
    ~finish:(fun _ -> None)

  let bingoed_col board =
    Array.fold_until board ~init:0 ~f:(fun col_idx _ ->
      let col = Array.init 5 ~f:(fun i -> board.(i).(col_idx)) in
      if all_marked col then Stop (Some col_idx)
      else Continue (col_idx + 1))
    ~finish:(fun _ -> None)

  let bingo board =
    match bingoed_row board with
    | Some ri -> `Row ri
    | None -> match bingoed_col board with
    | Some ci -> `Col ci
    | None -> `Unbingoed

end

let read path =
  let drawer_line :: board_lines = In_channel.read_all path
  |> String.split_lines
  |> List.filter ~f:(not << String.is_empty)
  in
  let drawer = String.split drawer_line ~on:',' |> List.map ~f:Int.of_string in
  let boards = board_lines
  |> List.groupi ~break:(fun i _ _ -> i % 5 = 0)
  |> List.map ~f:Board.of_lines
  in
  (drawer, boards)

let (drawer, boards) = read "inputs/04.txt"
let first_board = List.hd_exn boards
let row = first_board.(0)
