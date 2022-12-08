open Core

let small_input = {|30373
25512
65332
33549
35390|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day8.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.map ~f:Shared.string_to_chars
  |> List.map ~f:(List.map ~f:String.of_char)
  |> List.map ~f:(List.map ~f:Int.of_string)
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : int list list)];
  [%expect {|
    ((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0)) |}]
;;

let grid_size grid = List.length grid, List.length (List.hd_exn grid)
let get grid i j = List.nth_exn (List.nth_exn grid i) j

let check_coord grid i j =
  let n, m = grid_size grid in
  if Int.equal i 0 || Int.equal j 0 || Int.equal i (n - 1) || Int.equal j (m - 1)
  then true
  else (
    let v = List.nth_exn (List.nth_exn grid i) j in
    List.fold (List.init ~f:(fun n' -> get grid n' j < v) i) ~init:true ~f:( && )
    || List.fold (List.init ~f:(fun m' -> get grid i m' < v) j) ~init:true ~f:( && )
    || List.fold
         (List.init ~f:(fun n' -> get grid (i + n' + 1) j < v) (n - i - 1))
         ~init:true
         ~f:( && )
    || List.fold
         (List.init ~f:(fun m' -> get grid i (j + m' + 1) < v) (m - j - 1))
         ~init:true
         ~f:( && ))
;;

List.fold_until

let score_coord' grid i j =
  let n, m = grid_size grid in
  let v = List.nth_exn (List.nth_exn grid i) j in
  let up =
    List.fold_until
      ~init:0
      ~f:(fun accm n' ->
        if get grid n' j < v then Continue (accm + 1) else Stop (accm + 1))
      ~finish:Fn.id
      (List.rev (List.init ~f:Fn.id i))
  in
  let left =
    List.fold_until
      ~init:0
      ~f:(fun accm m' ->
        if get grid i m' < v then Continue (accm + 1) else Stop (accm + 1))
      ~finish:Fn.id
      (List.rev (List.init ~f:Fn.id j))
  in
  let down =
    List.fold_until
      ~init:0
      ~f:(fun accm n' ->
        if get grid (i + n' + 1) j < v then Continue (accm + 1) else Stop (accm + 1))
      ~finish:Fn.id
      (List.init ~f:Fn.id (n - i - 1))
  in
  let right =
    List.fold_until
      ~init:0
      ~f:(fun accm m' ->
        if get grid i (j + m' + 1) < v then Continue (accm + 1) else Stop (accm + 1))
      ~finish:Fn.id
      (List.init ~f:Fn.id (m - j - 1))
  in
  up, left, right, down
;;

let score_coord grid i j =
  let up, left, right, down = score_coord' grid i j in
  up * left * right * down
;;

let small_grid = parse small_input

let%expect_test "check_coord" =
  print_s
    [%sexp
      ([ check_coord small_grid 0 0
       ; check_coord small_grid 1 1
       ; check_coord small_grid 1 2
       ; check_coord small_grid 2 1
       ; check_coord small_grid 2 3
       ]
        : bool list)];
  [%expect {| (true true true true true) |}]
;;

let%expect_test "score_coord directional" =
  print_s
    [%sexp
      ([ score_coord' small_grid 1 2; score_coord' small_grid 3 2 ]
        : (int * int * int * int) list)];
  [%expect {| ((1 1 2 2) (2 2 2 1)) |}]
;;

let%expect_test "score_coord" =
  print_s [%sexp ([ score_coord small_grid 1 2; score_coord small_grid 3 2 ] : int list)];
  [%expect {| (4 8) |}]
;;

module Part1 = struct
  let solve' input =
    let grid = parse input in
    let n, m = grid_size grid in
    List.cartesian_product
      (List.tl_exn (List.init ~f:Fn.id (n - 1)))
      (List.tl_exn (List.init ~f:Fn.id (m - 1)))
    |> List.filter ~f:(fun (i, j) -> check_coord grid i j)
  ;;

  let solve input =
    let grid = parse input in
    let n, m = grid_size grid in
    let inners = input |> solve' |> List.length in
    inners + (2 * (n + m)) - 4
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : (int * int) list)];
    [%expect {| ((1 1) (1 2) (2 1) (2 3) (3 2)) |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 21 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 1809 |}]
  ;;
end

module Part2 = struct
  let solve' input =
    let grid = parse input in
    let n, m = grid_size grid in
    List.cartesian_product (List.init ~f:Fn.id n) (List.init ~f:Fn.id m)
    |> List.map ~f:(fun (i, j) -> score_coord grid i j)
  ;;

  let solve input = input |> solve' |> List.max_elt ~compare:Int.compare

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : int list)];
    [%expect {| (0 0 0 0 0 0 1 4 1 0 0 6 1 2 0 0 1 8 3 0 0 0 0 0 0) |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int option)];
    [%expect {| (8) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {| (479400) |}]
  ;;
end