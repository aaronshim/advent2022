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
  let fully_contains l1 l2 =
    match l1, l2 with
    | x1 :: x2 :: [], x3 :: x4 :: [] -> (x1 <= x3 && x4 <= x2) || (x3 <= x1 && x2 <= x4)
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
    |> List.filter ~f:(fun x -> fully_contains (List.nth_exn x 0) (List.nth_exn x 1))
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
  let any_overlap l1 l2 =
    match l1, l2 with
    | x1 :: x2 :: _, x3 :: x4 :: _ -> (x1 <= x3 && x3 <= x2) || (x3 <= x1 && x1 <= x4)
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
    |> List.filter ~f:(fun x -> any_overlap (List.nth_exn x 0) (List.nth_exn x 1))
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