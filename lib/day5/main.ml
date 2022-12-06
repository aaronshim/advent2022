open Core

let small_input =
  {|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day5.txt"

let parse_line stacks line =
  let rec parse_line' i n =
    if i >= String.length line
    then stacks
    else (
      let c_at = line.[i] in
      if Char.equal c_at ' ' then () else stacks.(n) <- c_at :: stacks.(n);
      parse_line' (i + 4) (n + 1))
  in
  parse_line' 1 0
;;

let parse_first_part lines =
  let num_buckets, lines' =
    String.length (List.last_exn lines) / 3, List.rev (List.tl_exn (List.rev lines))
  in
  let stacks = Array.of_list (List.init num_buckets ~f:(const [])) in
  List.fold ~init:stacks ~f:parse_line lines'
;;

let parse input =
  let parts =
    input |> String.split_lines |> List.group ~break:(fun _ y -> String.is_empty y)
  in
  let first_part = List.nth_exn parts 0 |> parse_first_part |> Array.map ~f:List.rev in
  let second_part =
    List.nth_exn parts 1
    |> List.tl_exn
    |> List.map ~f:(String.split_on_chars ~on:[ ' ' ])
    |> List.map ~f:(fun x ->
         ( Int.of_string (List.nth_exn x 1)
         , Int.of_string (List.nth_exn x 3)
         , Int.of_string (List.nth_exn x 5) ))
  in
  first_part, second_part
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : char list array * (int * int * int) list)];
  [%expect {|
    (((N Z) (D C M) (P)) ((1 2 1) (3 1 3) (2 2 1) (1 1 2))) |}]
;;

(* Parametric on the move function. *)
let move_all move_f (stacks : char list array) (moves : (int * int * int) list) =
  List.fold
    ~init:stacks
    ~f:(fun accm x -> move_f accm (Tuple3.get1 x) (Tuple3.get2 x - 1) (Tuple3.get3 x - 1))
    moves
;;

(* Solve function is polymorphic on the move function. *)
let solve' move_f input =
  let stacks, moves = parse input in
  let stacks' = move_all move_f stacks moves in
  stacks'
  |> Array.map ~f:List.hd
  |> Array.filter_opt
  |> Array.to_list
  |> String.of_char_list
;;

module Part1 = struct
  let move stacks num from_ to_ =
    let to_add, new_from = List.split_n stacks.(from_) num in
    let new_to = List.fold ~init:stacks.(to_) ~f:(fun accm x -> x :: accm) to_add in
    stacks.(from_) <- new_from;
    stacks.(to_) <- new_to;
    stacks
  ;;

  let solve = solve' move

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : string)];
    [%expect {| CMZ |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : string)];
    [%expect {| VGBBJCRMN |}]
  ;;
end

module Part2 = struct
  let move stacks num from_ to_ =
    let to_add, new_from = List.split_n stacks.(from_) num in
    let new_to = List.concat [ to_add; stacks.(to_) ] in
    stacks.(from_) <- new_from;
    stacks.(to_) <- new_to;
    stacks
  ;;

  let solve = solve' move

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : string)];
    [%expect {| MCD |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : string)];
    [%expect {| LBBVJBRMH |}]
  ;;
end
