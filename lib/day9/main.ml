open Core

let small_input = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day9.txt"

type direction =
  | Up of int
  | Down of int
  | Left of int
  | Right of int
[@@deriving sexp]

let parse_line line =
  match String.split_on_chars ~on:[ ' ' ] line with
  | [ "U"; n ] -> Some (Up (Int.of_string n))
  | [ "D"; n ] -> Some (Down (Int.of_string n))
  | [ "L"; n ] -> Some (Left (Int.of_string n))
  | [ "R"; n ] -> Some (Right (Int.of_string n))
  | _ -> None
;;

let parse input =
  input |> String.split_lines |> List.map ~f:String.strip |> List.filter_map ~f:parse_line
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : direction list)];
  [%expect
    {|
    ((Right 4) (Up 4) (Left 3) (Down 1) (Right 4) (Down 1) (Left 5) (Right 2)) |}]
;;

let is_done_moving_in_direction d =
  match d with
  | Up n -> n = 0
  | Down n -> n = 0
  | Left n -> n = 0
  | Right n -> n = 0
;;

(** Only move the tail if it is not adjacent. *)
let is_tail_ok (x_h, y_h) (x_t, y_t) = abs (x_h - x_t) <= 1 && abs (y_h - y_t) <= 1

let%expect_test "is_tail_ok" =
  print_s
    [%sexp
      (List.map
         ~f:(fun (e1, e2) -> is_tail_ok e1 e2)
         [ (1, 1), (2, 1); (1, 2), (2, 1); (1, 1), (1, 1) ]
        : bool list)];
  [%expect {| (true true true) |}]
;;

(** Move either one square in one direction or one square in both directions. *)
let move_tail (x_h, y_h) (x_t, y_t) =
  let x_t' = if x_h - x_t > 0 then x_t + 1 else if x_h - x_t < 0 then x_t - 1 else x_t in
  let y_t' = if y_h - y_t > 0 then y_t + 1 else if y_h - y_t < 0 then y_t - 1 else y_t in
  x_t', y_t'
;;

let%expect_test "move_tail" =
  print_s
    [%sexp
      (List.map
         ~f:(fun (e1, e2) -> move_tail e1 e2)
         [ (3, 1), (1, 1); (1, 1), (1, 3); (2, 3), (1, 1); (3, 2), (1, 1) ]
        : (int * int) list)];
  [%expect {| ((2 1) (1 2) (2 2) (2 2)) |}]
;;

(** For folding. *)
let rec f ((x_h, y_h), (x_t, y_t), visited) direction =
  if is_done_moving_in_direction direction
  then (x_h, y_h), (x_t, y_t), visited
  else (
    let x_h', y_h', direction' =
      match direction with
      | Up n -> x_h, y_h + 1, Up (n - 1)
      | Down n -> x_h, y_h - 1, Down (n - 1)
      | Left n -> x_h - 1, y_h, Left (n - 1)
      | Right n -> x_h + 1, y_h, Right (n - 1)
    in
    let x_t', y_t', visited' =
      if is_tail_ok (x_h', y_h') (x_t, y_t)
      then x_t, y_t, visited
      else (
        let x_t'', y_t'' = move_tail (x_h', y_h') (x_t, y_t) in
        x_t'', y_t'', (x_t'', y_t'') :: visited)
    in
    f ((x_h', y_h'), (x_t', y_t'), visited') direction')
;;

module Part1 = struct
  let solve' input =
    input
    |> parse
    |> List.fold ~init:((0, 0), (0, 0), []) ~f
    |> Tuple3.get3
    |> List.sort ~compare:(fun (x1, y1) (x2, y2) -> Int.compare x1 x2 + Int.compare y1 y2)
    |> List.remove_consecutive_duplicates ~equal:(fun (x1, y1) (x2, y2) ->
         x1 = x2 && y1 = y2)
  ;;

  let solve input = input |> solve' |> List.length

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : (int * int) list)];
    [%expect
      {|
      ((1 0) (1 2) (2 0) (2 2) (3 2) (3 3) (4 3) (2 4) (3 0) (3 4) (4 1) (4 2)
       (4 3)) |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {|
      13 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 7521 |}]
  ;;
end
