open Core

let small_input =
  {|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|}
;;

type operation =
  | Plus of int
  | Times of int
  | PlusOld
  | TimesOld
[@@deriving sexp]

type monkey = int list * operation * int * int * int * int [@@deriving sexp]

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day11.txt"

let parse_op line =
  let pieces = line |> String.strip |> String.split_on_chars ~on:[ ' ' ] |> List.rev in
  (* print_endline line; *)
  (* print_s [%sexp (pieces : string list)]; *)
  let n = List.nth_exn pieces 0 in
  match
    Shared.string_to_chars n |> List.for_all ~f:Char.is_digit, List.nth_exn pieces 1
  with
  | true, "+" -> Some (Plus (Int.of_string n))
  | true, "*" -> Some (Times (Int.of_string n))
  | false, "+" -> Some PlusOld
  | false, "*" -> Some TimesOld
  | _ -> None
;;

let parse_monkey lines : monkey =
  let _id = List.nth_exn lines 0 |> Shared.extract_ints |> List.hd_exn in
  let items = List.nth_exn lines 1 |> Shared.extract_ints in
  let op = List.nth_exn lines 2 |> parse_op |> Option.value_exn in
  let test = List.nth_exn lines 3 |> Shared.extract_ints |> List.hd_exn in
  let test_true = List.nth_exn lines 4 |> Shared.extract_ints |> List.hd_exn in
  let test_false = List.nth_exn lines 5 |> Shared.extract_ints |> List.hd_exn in
  items, op, test, test_true, test_false, 0
;;

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.group ~break:(fun _ x -> String.is_empty x)
  |> List.map ~f:(List.filter ~f:(fun x -> not (String.is_empty x)))
  |> List.map ~f:parse_monkey
  |> Array.of_list
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : monkey array)];
  [%expect
    {|
    (((79 98) (Times 19) 23 2 3 0) ((54 65 75 74) (Plus 6) 19 2 0 0)
     ((79 60 97) TimesOld 13 1 3 0) ((74) (Plus 3) 17 0 1 0)) |}]
;;

let%expect_test "Parse full input" =
  print_s [%sexp (parse full_input : monkey array)];
  [%expect
    {|
    (((53 89 62 57 74 51 83 97) (Times 3) 13 1 5 0)
     ((85 94 97 92 56) (Plus 2) 19 5 2 0) ((86 82 82) (Plus 1) 11 3 4 0)
     ((94 68) (Plus 5) 17 7 6 0) ((83 62 74 58 96 68 85) (Plus 4) 3 3 6 0)
     ((50 68 95 82) (Plus 8) 7 2 4 0) ((75) (Times 7) 5 7 0 0)
     ((92 52 85 89 68 82) TimesOld 2 0 1 0)) |}]
;;

(** Print out just the items. *)
let display_monkeys = Array.map ~f:(fun (x, _, _, _, _, _) -> x)

let process_monkey (divide_by_3 : bool) (monkeys : monkey array) (i : int) : monkey array =
  let all_monkeys_lcm =
    monkeys
    |> Array.to_list
    |> List.map ~f:(fun (_, _, x, _, _, _) -> x)
    |> List.reduce_exn ~f:( * )
  in
  let items, op, test, test_true, test_false, num_inspected = monkeys.(i) in
  let rec process_monkey' items num_inspected' =
    match items with
    | [] ->
      monkeys.(i) <- [], op, test, test_true, test_false, num_inspected + num_inspected';
      monkeys
    (* Keep throwing until no more items are left. *)
    | item :: items' ->
      let worry_level =
        match op with
        | Plus n -> Int.( + ) n item
        | Times n -> Int.( * ) n item
        | PlusOld -> Int.( + ) item item
        | TimesOld -> Int.( * ) item item
      in
      let worry_level' =
        if divide_by_3
        then Int.( / ) worry_level 3
        else Int.( % ) worry_level all_monkeys_lcm
        (* else Int.( % ) worry_level 1 *)
      in
      let worry_level_check = Int.( = ) (Int.( % ) worry_level' test) 0 in
      let next_monkey = if worry_level_check then test_true else test_false in
      let items2, op2, test2, test_true2, test_false2, num_inspected2 =
        monkeys.(next_monkey)
      in
      (* print_string "Processing Monkey ";
      print_s [%sexp (i, op, test : int * operation * int)];
      print_string " with item ";
      print_s [%sexp (item : int)];
      print_string "Giving item ";
      print_s [%sexp (worry_level' : int)];
      print_string " to Monkey ";
      print_s [%sexp (next_monkey : int)]; *)
      monkeys.(next_monkey)
        <- ( List.append items2 [ worry_level' ]
           , op2
           , test2
           , test_true2
           , test_false2
           , num_inspected2 );
      process_monkey' items' (num_inspected' + 1)
  in
  process_monkey' items 0
;;

let%expect_test "First monkey goes" =
  print_s
    [%sexp
      (small_input |> parse |> (fun x -> process_monkey true x 0) |> display_monkeys
        : int list array)];
  [%expect {|
    (() (54 65 75 74) (79 60 97) (74 500 620)) |}]
;;

let do_turn_of_monkeys divide_by_3 monkeys =
  List.fold
    ~init:monkeys
    ~f:(process_monkey divide_by_3)
    (List.range 0 (Array.length monkeys))
;;

let%expect_test "Turn of monkeys" =
  print_s
    [%sexp
      (small_input |> parse |> do_turn_of_monkeys true |> display_monkeys
        : int list array)];
  [%expect {|
    ((20 23 27 26) (2080 25 167 207 401 1046) () ()) |}]
;;

module Part1 = struct
  let solve' input =
    List.fold
      ~init:(parse input)
      ~f:(fun x _ -> do_turn_of_monkeys true x)
      (List.range 0 20)
  ;;

  let solve input =
    let top_monkeys =
      input
      |> solve'
      |> Array.to_list
      |> List.map ~f:(fun (_, _, _, _, _, num_inspected) -> num_inspected)
      |> List.sort ~compare:Int.compare
      |> List.rev
    in
    List.nth_exn top_monkeys 0 * List.nth_exn top_monkeys 1
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : monkey array)];
    [%expect
      {|
      (((10 12 14 26 34) (Times 19) 23 2 3 101)
       ((245 93 53 199 115) (Plus 6) 19 2 0 95) (() TimesOld 13 1 3 7)
       (() (Plus 3) 17 0 1 105)) |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {|
      10605 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 110220 |}]
  ;;
end

module Part2 = struct
  let solve' n input =
    List.fold
      ~init:(parse input)
      ~f:(fun x _ -> do_turn_of_monkeys false x)
      (List.range 0 n)
  ;;

  let solve input =
    let top_monkeys =
      input
      |> solve' 10000
      |> Array.to_list
      |> List.map ~f:(fun (_, _, _, _, _, num_inspected) -> num_inspected)
      |> List.sort ~compare:Int.compare
      |> List.rev
    in
    List.nth_exn top_monkeys 0 * List.nth_exn top_monkeys 1
  ;;

  let%expect_test "Solve small input" =
    print_s
      [%sexp
        (List.map
           ~f:(fun n ->
             solve' n small_input
             |> Array.to_list
             |> List.map ~f:(fun (_, _, _, _, _, num_inspected) -> num_inspected))
           [ 1; 20; 1000 ]
          : int list list)];
    [%expect {|
      ((2 4 3 6) (99 97 8 103) (5204 4792 199 5192)) |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {|
      2713310158 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 19457438264 |}]
  ;;
end