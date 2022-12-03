open Core

let small_input = {|A Y
B X
C Z|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day3.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.map ~f:(String.split_on_chars ~on:[ ' ' ])
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : string list list)];
  [%expect {| ((A Y) (B X) (C Z)) |}]
;;

module Part1 = struct
  let solve input =
    input
    |> parse
    |> List.map ~f:(Shared.run_on_exactly_two score)
    |> List.map ~f:Option.join
    |> List.filter_opt
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 15 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 11666 |}]
  ;;
end

module Part2 = struct
  let solve input =
    input
    |> parse
    |> List.map ~f:(Shared.run_on_exactly_two score)
    |> List.map ~f:Option.join
    |> List.filter_opt
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 12 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 12767 |}]
  ;;
end