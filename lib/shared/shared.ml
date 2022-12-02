open Core

let sum_int = List.fold ~init:0 ~f:( + )

let%expect_test "sum_int correctly" =
  print_s [%sexp (sum_int [ 1; 2; 3 ] : int)];
  [%expect {| 6 |}]
;;
