open Base
open Core

let string_of_output = Int.to_string

let ( << ) = Fn.compose

type cell_state = Marked of int | Unmarked of int

type input = (cell_state array array list * int list)
type output = int

(********* FIXME worst brute force approach employed *)

module Board = struct

  let row_of_line =
    List.map ~f:(fun s -> Unmarked (Int.of_string s)) << Str.(split (regexp " +"))

  let of_lines = List.to_array << List.(map ~f:(to_array << row_of_line))

  let mark x board =
    let fold_result =
      Array.fold_until board
      ~init:0
      ~f:(fun row_idx row ->
        let found = Array.findi row ~f:(fun _ n ->
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
    | Some (r, c) -> begin
        match board.(r).(c) with
        | Unmarked x -> board.(r).(c) <- Marked x
        | _ -> ()
    end
    | _ -> ()

  let all_marked =
    Array.for_all ~f:(fun c ->
      match c with
      | Marked _ -> true
      | _ -> false)

  let continue_aux idx vec = 
    let open Continue_or_stop in
    if all_marked vec then Stop (Some idx) else Continue (idx + 1)

  let bingoed_row board =
    Array.fold_until board ~init:0 ~f:continue_aux ~finish:(fun _ -> None)

  let bingoed_col board =
    Array.fold_until board ~init:0 ~f:(fun col_idx _ ->
      let col = Array.init 5 ~f:(fun i -> board.(i).(col_idx)) in
      continue_aux col_idx col)
    ~finish:(fun _ -> None)

  let bingo board =
    match bingoed_row board with
    | Some ri -> `Row ri
    | None -> match bingoed_col board with
    | Some ci -> `Col ci
    | None -> `Unbingoed

  let value board =
    Array.fold board
    ~init:0
    ~f:(fun v row ->
      v + Array.fold row ~init:0 ~f:(fun s cell ->
        match cell with
        | Unmarked x -> s + x
        | _ -> s))

end

let read path =
  let parsed = In_channel.read_all path
  |> String.split_lines
  |> List.filter ~f:(not << String.is_empty) in
  let (drawer_line, board_lines) = List.(hd_exn parsed, tl_exn parsed) in
  let drawer = String.split drawer_line ~on:',' |> List.map ~f:Int.of_string in
  let boards = board_lines
  |> List.groupi ~break:(fun i _ _ -> i % 5 = 0)
  |> List.map ~f:Board.of_lines
  in
  (boards, drawer)

let bingoed_board boards n =
    List.fold_until boards
    ~init:0
    ~f:(fun idx board ->
      Board.mark n board;
      match Board.bingo board with
      | `Unbingoed -> Continue (idx + 1)
      | _ -> Stop (Some board))
    ~finish:(fun _ -> None)

let solve_one (boards, drawer) =
  let res = List.fold_until drawer
  ~init:0
  ~f:(fun idx n ->
    match bingoed_board boards n with
    | Some x -> Stop (Some (x, n))
    | _ -> Continue (idx + 1))
  ~finish:(fun _ -> None)
  in
  match res with
  | Some (board, v) -> (Board.value board) * v
  | None -> 0

let solve_two (boards, drawer) =
  let (last_board, n) = List.fold_until drawer
  ~init:(boards, drawer)
  ~f:(fun (boards, drawer) n ->
    match List.rev_filter boards ~f:(fun board ->
      Board.mark n board;
      match Board.bingo board with
      | `Unbingoed -> true
      | _ -> false)
    with
    | [] -> failwith "No suitable board"
    | [board] -> Stop (board, List.(drawer |> tl_exn |> hd_exn))
    | boards -> Continue (boards, List.tl_exn drawer))
  ~finish:(fun _ -> failwith "No suitable board")
  in
  Board.mark n last_board;
  n * Board.value last_board
