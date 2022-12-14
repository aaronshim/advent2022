open Core

let small_input = {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day4.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.map ~f:(String.split_on_chars ~on:[ ',' ])
  |> List.map ~f:(List.map ~f:(String.split_on_chars ~on:[ '-' ]))
  |> List.map ~f:(List.map ~f:(List.map ~f:Int.of_string))
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : int list list list)];
  [%expect
    {|
    (((2 4) (6 8)) ((2 3) (4 5)) ((5 7) (7 9)) ((2 8) (3 7)) ((6 6) (4 6))
     ((2 6) (4 8))) |}]
;;

module Part1 = struct
  let fully_contains interval1 interval2 =
    match interval1, interval2 with
    | [ left1; right1 ], [ left2; right2 ] -> (left1 <= left2 && right2 <= right1) || (left2 <= left1 && right1 <= right2)
    | _, _ -> false
  ;;

  let%expect_test "Fully contains test" =
    print_s
      [%sexp
        ([ fully_contains [ 2; 8 ] [ 3; 7 ]; fully_contains [ 2; 4 ] [ 6; 8 ] ]
          : bool list)];
    [%expect {|
      (true false) |}]
  ;;

  let solve input =
    input
    |> parse
    |> List.map ~f:(Shared.run_on_exactly_two fully_contains)
    |> List.filter_opt
    |> List.filter ~f:Fun.id
    |> List.length
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 2 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 576 |}]
  ;;
end

module Part2 = struct
  let any_overlap interval1 interval2 =
    match interval1, interval2 with
    | [ left1; right1 ], [ left2; right2 ] -> (left1 <= left2 && left2 <= right1) || (left2 <= left1 && left1 <= right2)
    | _, _ -> false
  ;;

  let%expect_test "Any overlap test" =
    print_s
      [%sexp
        ([ any_overlap [ 5; 7 ] [ 7; 9 ]; any_overlap [ 2; 4 ] [ 6; 8 ] ] : bool list)];
    [%expect {|
      (true false) |}]
  ;;

  let solve input =
    input
    |> parse
    |> List.map ~f:(Shared.run_on_exactly_two any_overlap)
    |> List.filter_opt
    |> List.filter ~f:Fun.id
    |> List.length
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 4 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 905 |}]
  ;;
end