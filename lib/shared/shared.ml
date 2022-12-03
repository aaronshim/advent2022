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