open Core

let small_input = {|mjqjpqmgbljsphdztnvjfqwrcgsmlb|}
let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day6.txt"
let parse input = input |> String.strip |> Shared.string_to_chars

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : char list)];
  [%expect {|
    (m j q j p q m g b l j s p h d z t n v j f q w r c g s m l b) |}]
;;

let rec check_n n i xs =
  if i + n >= List.length xs
  then -1
  else if i + n < List.length xs
          && Set.length (Char.Set.of_list (List.sub xs ~pos:i ~len:n)) = n
  then i + n
  else check_n n (i + 1) xs
;;

module Part1 = struct
  let check_four = check_n 4
  let solve input = input |> parse |> check_four 0

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 7 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "bvwbjplbgvbhsrlpgdmjqwftvncz" : int)];
    [%expect {| 5 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "nppdvjthqldpwncqszvftbrmjlhg" : int)];
    [%expect {| 6 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" : int)];
    [%expect {| 10 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" : int)];
    [%expect {| 11 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 1625 |}]
  ;;
end

module Part2 = struct
  let solve input = input |> parse |> check_n 14 0

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 19 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "bvwbjplbgvbhsrlpgdmjqwftvncz" : int)];
    [%expect {| 23 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "nppdvjthqldpwncqszvftbrmjlhg" : int)];
    [%expect {| 23 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" : int)];
    [%expect {| 29 |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" : int)];
    [%expect {| 26 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 2250 |}]
  ;;
end
