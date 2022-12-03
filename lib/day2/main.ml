open Core

let small_input = {|A Y
B X
C Z|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day2.txt"

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
  let shape_score you =
    match you with
    | "X" -> Some 1
    | "Y" -> Some 2
    | "Z" -> Some 3
    | _ -> None
  ;;

  let win_score them you =
    match them, you with
    | "A", "X" -> Some 3
    | "A", "Y" -> Some 6
    | "A", "Z" -> Some 0
    | "B", "X" -> Some 0
    | "B", "Y" -> Some 3
    | "B", "Z" -> Some 6
    | "C", "X" -> Some 6
    | "C", "Y" -> Some 0
    | "C", "Z" -> Some 3
    | _ -> None
  ;;

  let score them you = Option.map2 ~f:( + ) (shape_score you) (win_score them you)

  let%expect_test "Scores" =
    print_s [%sexp ([ score "A" "Y"; score "B" "X"; score "C" "Z" ] : int option list)];
    [%expect {| ((8) (1) (6)) |}]
  ;;

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
  let proper_shape them you =
    match them, you with
    | "A", "X" -> Some "C"
    | "A", "Y" -> Some "A"
    | "A", "Z" -> Some "B"
    | "B", "X" -> Some "A"
    | "B", "Y" -> Some "B"
    | "B", "Z" -> Some "C"
    | "C", "X" -> Some "B"
    | "C", "Y" -> Some "C"
    | "C", "Z" -> Some "A"
    | _ -> None
  ;;

  let%expect_test "Proper shapes responding to correct outcomes" =
    print_s
      [%sexp
        ([ proper_shape "A" "Y"; proper_shape "B" "X"; proper_shape "C" "Z" ]
          : string option list)];
    [%expect {| ((A) (A) (A)) |}]
  ;;

  let win_score you =
    match you with
    | "X" -> Some 0
    | "Y" -> Some 3
    | "Z" -> Some 6
    | _ -> None
  ;;

  let shape_score you =
    match you with
    | "A" -> Some 1
    | "B" -> Some 2
    | "C" -> Some 3
    | _ -> None
  ;;

  let score them you =
    Option.map2
      ~f:( + )
      (Option.( >>= ) (proper_shape them you) shape_score)
      (win_score you)
  ;;

  let%expect_test "Scores" =
    print_s [%sexp ([ score "A" "Y"; score "B" "X"; score "C" "Z" ] : int option list)];
    [%expect {| ((4) (1) (7)) |}]
  ;;

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