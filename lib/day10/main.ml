open Core

let small_input = {|noop
addx 3
addx -5|}

let smallish_input =
  {|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|}
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day10.txt"

type instruction =
  | Noop
  | Addx of int
[@@deriving sexp]

let parse_line line =
  match String.split_on_chars ~on:[ ' ' ] line with
  | [ "noop" ] -> Some Noop
  | [ "addx"; n ] -> Some (Addx (Int.of_string n))
  | _ -> None
;;

let parse input =
  input |> String.split_lines |> List.map ~f:String.strip |> List.filter_map ~f:parse_line
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : instruction list)];
  [%expect {|
    (Noop (Addx 3) (Addx -5)) |}]
;;

(** For folding, will store X register changes *at the start of* a particular cycle. *)
let f (clock, past_states) inst =
  let _clock, x = List.hd_exn past_states in
  match inst with
  | Noop -> clock + 1, past_states
  | Addx n -> clock + 2, (clock + 2, x + n) :: past_states
;;

(** Clock cycles will start at 1, X register will start at 1. *)
let run_program parsed = List.fold ~init:(1, [ 0, 1 ]) ~f parsed

(** Run through state changes until we get to the part that happens after the moment we're interested in. *)
let x_during states clock =
  let states_definitely_sorted =
    List.sort ~compare:(fun (clock1, _) (clock2, _) -> Int.compare clock1 clock2) states
  in
  List.fold_until
    ~init:None
    ~finish:Fn.id
    ~f:(fun accm (clock', x) -> if clock' <= clock then Continue (Some x) else Stop accm)
    states_definitely_sorted
;;

module Part1 = struct
  let solve' input = input |> parse |> run_program

  let%expect_test "Run program of small input" =
    print_s [%sexp (solve' small_input : int * (int * int) list)];
    [%expect {|
      (6 ((6 -1) (4 4) (0 1))) |}]
  ;;

  let%expect_test "Run program of small input" =
    let clock, state = solve' small_input in
    print_s
      [%sexp
        (List.range 1 clock ~stop:`inclusive |> List.map ~f:(x_during state)
          : int option list)];
    [%expect {|
      ((1) (1) (1) (4) (4) (-1)) |}]
  ;;

  let solve input =
    let clock, states = input |> solve' in
    let cycles = List.range ~stride:40 20 clock in
    let xs = cycles |> List.map ~f:(x_during states) |> List.filter_opt in
    Shared.sum_int (List.map2_exn ~f:(fun x y -> x * y) cycles xs)
  ;;

  let%expect_test "Solve smallish input" =
    print_s [%sexp (solve smallish_input : int)];
    [%expect {|
      13140 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 17380 |}]
  ;;
end

module Part2 = struct
  (** Clock cycles are 1-indexed but screens are 0-indexed. *)
  let pixel_during_cycle n =
    let n' = n % 40 in
    (if n' = 0 then 40 else n') - 1
  ;;

  let crt parsed =
    let _, states = run_program parsed in
    let f grid clock =
      let p = pixel_during_cycle clock in
      (* Clock cycles are 1-indexed but screens are 0-indexed. *)
      let row = (clock - 1) / 40 in
      let sprite = Option.value_exn (x_during states clock) in
      (* print_s [%sexp (row, p : int * int)]; *)
      (* If the current CRT pixel being drawn is within 1 of the center of the sprite, draw! *)
      grid.(row).(p) <- (if abs (sprite - p) <= 1 then "#" else ".");
      grid
    in
    let new_grid = Array.init 6 ~f:(fun _ -> Array.init 40 ~f:(fun _ -> " ")) in
    List.range 1 240 ~stop:`inclusive
    |> List.fold ~init:new_grid ~f
    |> Array.map ~f:(String.concat_array ~sep:"")
    |> String.concat_array ~sep:"\n"
  ;;

  let solve input = input |> parse |> crt

  let%expect_test "Solve smallish input" =
    print_string (solve smallish_input);
    [%expect
      {|
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######..... |}]
  ;;

  let%expect_test "Solve full input" =
    print_string (solve full_input);
    [%expect
      {|
    ####..##...##..#..#.####.###..####..##..
    #....#..#.#..#.#..#....#.#..#.#....#..#.
    ###..#....#....#..#...#..#..#.###..#....
    #....#.##.#....#..#..#...###..#....#....
    #....#..#.#..#.#..#.#....#.#..#....#..#.
    #.....###..##...##..####.#..#.####..##.. |}]
  ;;
end