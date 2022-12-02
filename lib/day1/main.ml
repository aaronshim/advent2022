open Core

let small_input =
  {|1000
  2000
  3000
  
  4000
  
  5000
  6000
  
  7000
  8000
  9000
  
  10000|}
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day1.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> String.concat ~sep:"\n"
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:String.split_lines
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : int list list)];
  [%expect {| ((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000)) |}]
;;

module Part1 = struct
  let solve input =
    input
    |> parse
    |> List.map ~f:Shared.sum_int
    |> List.max_elt ~compare:Int.compare
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int option)];
    [%expect {| (24000) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {| (69501) |}]
  ;;
end

module Part2 = struct
  let solve input =
    let calories =
      input
      |> parse
      |> List.map ~f:(Shared.sum_int)
      |> List.sort ~compare:Int.compare
      |> List.rev
    in
    List.take calories 3 |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 45000 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 202346 |}]
  ;;
end