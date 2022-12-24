open Core

let small_input =
  {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|}
;;

type elem =
  | Lst of elem list
  | Nm of int
[@@deriving sexp]

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day13.txt"

let split_each_line line =
  line
  |> Shared.string_to_chars
  |> List.group ~break:(fun a b -> not (Char.is_digit a && Char.is_digit b))
  |> List.filter ~f:(fun a -> not (List.equal Char.equal a [ ',' ]))
  |> List.map ~f:String.of_char_list
  |> Shared.remove_first_and_last
;;

let%expect_test "Split each line" =
  print_s [%sexp ([ "[[1],[12,3,4]]" ] |> List.map ~f:split_each_line : string list list)];
  [%expect {|
    (([ 1 ] [ 12 3 4 ])) |}]
;;

let rec parse_elem input =
  let rec parse_elem' intermediate chunks =
    match chunks with
    | [] -> intermediate
    | chunk :: rest ->
      (match chunk with
       (* Pass over closing brackets that we must have consumed the innards of earlier. *)
       | "]" -> parse_elem' intermediate rest
       | "[" ->
         let subchunk, rest' =
           List.split_while ~f:(fun x -> not (String.equal "]" x)) rest
         in
         (* print_s [%sexp (rest, subchunk, rest' : string list * string list * string list)]; *)
         parse_elem' (parse_elem subchunk :: intermediate) rest'
       | digits -> parse_elem' (Nm (Int.of_string digits) :: intermediate) rest)
  in
  parse_elem' [] input |> List.rev |> Lst
;;

let%expect_test "Parse line" =
  print_s
    [%sexp
      ([ "[[1],[12,3,4]]" ] |> List.map ~f:(fun x -> x |> split_each_line |> parse_elem)
        : elem list)];
  [%expect {|
    ((Lst ((Lst ((Nm 1))) (Lst ((Nm 12) (Nm 3) (Nm 4)))))) |}]
;;

(* let rec parse_elem str = str *)

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.group ~break:(fun _ x -> String.is_empty x)
  |> List.map ~f:(List.filter ~f:(fun x -> not (String.is_empty x)))
  |> List.map ~f:(fun xs ->
       ( List.nth_exn xs 0 |> split_each_line |> parse_elem
       , List.nth_exn xs 1 |> split_each_line |> parse_elem ))
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : (elem * elem) list)];
  [%expect
    {|
    (((Lst ((Nm 1) (Nm 1) (Nm 3) (Nm 1) (Nm 1)))
      (Lst ((Nm 1) (Nm 1) (Nm 5) (Nm 1) (Nm 1))))
     ((Lst ((Lst ((Nm 1))) (Lst ((Nm 2) (Nm 3) (Nm 4)))))
      (Lst ((Lst ((Nm 1))) (Nm 4))))
     ((Lst ((Nm 9))) (Lst ((Lst ((Nm 8) (Nm 7) (Nm 6))))))
     ((Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4)))
      (Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4) (Nm 4))))
     ((Lst ((Nm 7) (Nm 7) (Nm 7) (Nm 7))) (Lst ((Nm 7) (Nm 7) (Nm 7))))
     ((Lst ()) (Lst ((Nm 3)))) ((Lst ((Lst ((Lst ()))))) (Lst ((Lst ()))))
     ((Lst
       ((Nm 1)
        (Lst ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 7)))))))))
        (Nm 8) (Nm 9)))
      (Lst
       ((Nm 1)
        (Lst ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 0)))))))))
        (Nm 8) (Nm 9))))) |}]
;;

type judgement =
  | Correct
  | Wrong
  | Equal
[@@deriving sexp]

let rec right_order a b =
  match a, b with
  | [], [] -> Equal
  | [], _ -> Correct
  | _, [] -> Wrong
  | left :: a', right :: b' ->
    let compare =
      match left, right with
      | Nm l, Nm r -> if l < r then Correct else if l > r then Wrong else Equal
      | Lst l, Lst r -> right_order l r
      | Nm l, Lst r -> right_order [ Nm l ] r
      | Lst l, Nm r -> right_order l [ Nm r ]
    in
    (match compare with
     | Equal -> right_order a' b'
     | _ -> compare)
;;

let force_lst x =
  match x with
  | Lst x' -> x'
  | _ -> []
;;

let%expect_test "Right order" =
  let a = "[1,[2,[3,[4,[5,6,7]]]],8,9]" |> split_each_line |> parse_elem |> force_lst in
  let b = "[1,[2,[3,[4,[5,6,0]]]],8,9]" |> split_each_line |> parse_elem |> force_lst in
  print_s [%sexp (right_order a b : judgement)];
  [%expect {|
    Wrong |}]
;;

module Part1 = struct
  let solve' input =
    input
    |> parse
    |> List.mapi ~f:(fun i (a, b) ->
         i + 1, right_order (force_lst a) (force_lst b), (a, b))
  ;;

  let solve input =
    input
    |> solve'
    |> List.filter ~f:(fun (_, x, _) ->
         match x with
         | Correct -> true
         | _ -> false)
    |> List.map ~f:Tuple3.get1
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : (int * judgement * (elem * elem)) list)];
    print_s [%sexp (solve small_input : int)];
    [%expect
      {|
      ((1 Correct
        ((Lst ((Nm 1) (Nm 1) (Nm 3) (Nm 1) (Nm 1)))
         (Lst ((Nm 1) (Nm 1) (Nm 5) (Nm 1) (Nm 1)))))
       (2 Correct
        ((Lst ((Lst ((Nm 1))) (Lst ((Nm 2) (Nm 3) (Nm 4)))))
         (Lst ((Lst ((Nm 1))) (Nm 4)))))
       (3 Wrong ((Lst ((Nm 9))) (Lst ((Lst ((Nm 8) (Nm 7) (Nm 6)))))))
       (4 Correct
        ((Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4)))
         (Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4) (Nm 4)))))
       (5 Wrong ((Lst ((Nm 7) (Nm 7) (Nm 7) (Nm 7))) (Lst ((Nm 7) (Nm 7) (Nm 7)))))
       (6 Correct ((Lst ()) (Lst ((Nm 3)))))
       (7 Wrong ((Lst ((Lst ((Lst ()))))) (Lst ((Lst ())))))
       (8 Wrong
        ((Lst
          ((Nm 1)
           (Lst
            ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 7)))))))))
           (Nm 8) (Nm 9)))
         (Lst
          ((Nm 1)
           (Lst
            ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 0)))))))))
           (Nm 8) (Nm 9))))))
      13 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 6070 |}]
  ;;
end

module Part2 = struct
  let solve' input =
    input
    |> parse
    |> List.map ~f:(fun (a, b) -> [ a; b ])
    |> List.append [ [ Lst [ Nm 2 ]; Lst [ Nm 6 ] ] ]
    (* |> List.append [[ "[[2]]" |> split_each_line |> parse_elem ];["[[6]]" |> split_each_line |> parse_elem ]] *)
    |> List.concat
    |> List.sort ~compare:(fun a b ->
         match right_order (force_lst a) (force_lst b) with
         | Correct -> -1
         | Wrong -> 1
         | Equal -> 0)
  ;;

  let solve input =
    input
    |> solve'
    |> List.filter_mapi ~f:(fun i x ->
         match x with
         | Lst [ Nm 2 ] -> Some (i + 1)
         | Lst [ Nm 6 ] -> Some (i + 1)
         | _ -> None)
    |> List.reduce ~f:Int.( * )
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : elem list)];
    print_s [%sexp (solve small_input : int option)];
    [%expect
      {|
      ((Lst ()) (Lst ((Lst ()))) (Lst ((Lst ((Lst ())))))
       (Lst ((Nm 1) (Nm 1) (Nm 3) (Nm 1) (Nm 1)))
       (Lst ((Nm 1) (Nm 1) (Nm 5) (Nm 1) (Nm 1)))
       (Lst ((Lst ((Nm 1))) (Lst ((Nm 2) (Nm 3) (Nm 4)))))
       (Lst
        ((Nm 1)
         (Lst ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 0)))))))))
         (Nm 8) (Nm 9)))
       (Lst
        ((Nm 1)
         (Lst ((Nm 2) (Lst ((Nm 3) (Lst ((Nm 4) (Lst ((Nm 5) (Nm 6) (Nm 7)))))))))
         (Nm 8) (Nm 9)))
       (Lst ((Lst ((Nm 1))) (Nm 4))) (Lst ((Nm 2))) (Lst ((Nm 3)))
       (Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4)))
       (Lst ((Lst ((Nm 4) (Nm 4))) (Nm 4) (Nm 4) (Nm 4))) (Lst ((Nm 6)))
       (Lst ((Nm 7) (Nm 7) (Nm 7))) (Lst ((Nm 7) (Nm 7) (Nm 7) (Nm 7)))
       (Lst ((Lst ((Nm 8) (Nm 7) (Nm 6))))) (Lst ((Nm 9))))
      (140) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {| (20758) |}]
  ;;
end