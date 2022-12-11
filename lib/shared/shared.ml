open Core

let sum_int = List.fold ~init:0 ~f:( + )

let%expect_test "sum_int correctly" =
  print_s [%sexp (sum_int [ 1; 2; 3 ] : int)];
  [%expect {| 6 |}]
;;

let run_on_exactly_two f xs =
  match xs with
  | [ x; y ] -> Some (f x y)
  | _ -> None
;;

let%expect_test "run_on_exactly_two (two)" =
  print_s [%sexp (run_on_exactly_two ( + ) [ 1; 2 ] : int option)];
  [%expect {| (3) |}]
;;

let%expect_test "run_on_exactly_two (not two)" =
  print_s [%sexp (run_on_exactly_two ( + ) [ 1; 2; 3 ] : int option)];
  [%expect {| () |}]
;;

let string_to_chars s = List.init (String.length s) ~f:(String.get s)

let%expect_test "string_to_chars" =
  print_s [%sexp (string_to_chars "abc" : char list)];
  [%expect {| (a b c) |}]
;;

(* All ints in the string get returned. *)
let extract_ints str =
  str
  |> string_to_chars
  |> List.group ~break:(fun a b ->
       (Char.is_digit a && not (Char.is_digit b))
       || (Char.is_digit b && not (Char.is_digit a)))
  |> List.filter ~f:(List.for_all ~f:Char.is_digit)
  |> List.map ~f:(fun xs -> xs |> String.of_char_list |> Int.of_string)
;;

let%expect_test "extract_ints" =
  print_s
    [%sexp
      (List.map ~f:extract_ints [ "32;"; "1 + 2 = 3"; "Hello there 1 and 21" ]
        : int list list)];
  [%expect {| ((32) (1 2 3) (1 21)) |}]
;;