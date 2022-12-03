open Core

let small_input =
  {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|}
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day3.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  (* String to a list of chars *)
  |> List.map ~f:(fun s -> List.init (String.length s) ~f:(String.get s))
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : char list list)];
  [%expect
    {|
    ((v J r w p W t w J g W r h c s F M M f F F h F p)
     (j q H R N q R j q z j G D L G L r s F M f F Z S r L r F Z s S L)
     (P m m d z q P r V v P w w T W B w g)
     (w M q v L M Z H h H M v w L H j b v c j n n S B n v T Q F n)
     (t t g J t R G J Q c t T Z t Z T)
     (C r Z s J s P P Z s G z w w s L w L m p w M D w)) |}]
;;

let split_list_at n xs =
  let rec split' n xs accm =
    match n, xs, accm with
    | 0, xs', accm -> List.rev accm, xs'
    | _, [], accm -> List.rev accm, []
    | n, x :: xs, accm -> split' (n - 1) xs (x :: accm)
  in
  split' n xs []
;;

let%expect_test "Split list" =
  print_s [%sexp (split_list_at 3 [ 1; 2; 3; 4; 5 ] : int list * int list)];
  [%expect {|
    ((1 2 3) (4 5)) |}]
;;

let halve_list xs = split_list_at (List.length xs / 2) xs

let%expect_test "Halve list" =
  print_s [%sexp (halve_list [ 1; 2; 3; 4; 5; 6 ] : int list * int list)];
  [%expect {|
    ((1 2 3) (4 5 6)) |}]
;;

module Part1 = struct
  let find_repeat_in_second_half fst snd =
    let s = Char.Set.of_list fst in
    List.find snd ~f:(fun x -> Set.mem s x)
  ;;

  let char_priority c =
    if Char.is_uppercase c then Char.to_int c - 38 else Char.to_int c - 96
  ;;

  let%expect_test "Priority" =
    print_s
      [%sexp (List.map ~f:char_priority [ 'p'; 'L'; 'P'; 'v'; 't'; 's' ] : int list)];
    [%expect {|
    (16 38 42 22 20 19) |}]
  ;;

  let solve input =
    input
    |> parse
    |> List.map ~f:halve_list
    |> List.map ~f:(fun x -> find_repeat_in_second_half (fst x) (snd x))
    |> List.filter_opt
    |> List.map ~f:char_priority
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 157 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 8298 |}]
  ;;
end

module Part2 = struct
  (* Take a list of Set's and find the common elements. *)
  let i3 (xs : Char.Set.t list) : char list =
    List.fold ~init:(List.hd_exn xs) ~f:Set.inter xs |> Set.to_list
  ;;

  let solve input =
    input
    |> parse
    |> List.map ~f:Char.Set.of_list
    |> List.chunks_of ~length:3
    |> List.map ~f:i3
    |> List.map ~f:List.hd_exn (* Only one element per list. *)
    |> List.map ~f:Part1.char_priority
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 70 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 2708 |}]
  ;;
end