open Core

let small_input = {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|}

type elem =
  | Rock
  | Air
  | Sand
[@@deriving sexp, compare, equal]

let elem_to_char e =
  match e with
  | Rock -> '#'
  | Air -> '.'
  | Sand -> 'o'
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day14.txt"

let parse_line s =
  Shared.extract_ints s
  |> List.chunks_of ~length:2
  |> List.map ~f:(fun xs -> List.hd_exn xs, List.nth_exn xs 1)
;;

let%expect_test "Parse line" =
  print_s [%sexp (parse_line "503,4 -> 502,4 -> 502,9 -> 494,9" : (int * int) list)];
  [%expect {|
  ((503 4) (502 4) (502 9) (494 9)) |}]
;;

let parse input =
  input |> String.split_lines |> List.map ~f:String.strip |> List.map ~f:parse_line
;;

let%expect_test "Parse input" =
  print_s [%sexp (parse small_input : (int * int) list list)];
  [%expect {|
  (((498 4) (498 6) (496 6)) ((503 4) (502 4) (502 9) (494 9))) |}]
;;

let rec all_points_in_between xs (x2, y2) =
  (* print_s [%sexp (xs : (int * int) list)]; *)
  let x1, y1 = List.hd_exn xs in
  if x1 = x2 && y1 = y2
  then xs
  else if x1 = x2
  then
    if y1 < y2
    then all_points_in_between ((x1, y1 + 1) :: xs) (x2, y2)
    else all_points_in_between ((x1, y1 - 1) :: xs) (x2, y2)
  else if y1 = y2
  then
    if x1 < x2
    then all_points_in_between ((x1 + 1, y1) :: xs) (x2, y2)
    else all_points_in_between ((x1 - 1, y1) :: xs) (x2, y2)
  else []
;;

let%expect_test "all_points_in_between" =
  print_s [%sexp (all_points_in_between [ 498, 4 ] (498, 6) : (int * int) list)];
  print_s [%sexp (all_points_in_between [ 498, 6 ] (496, 6) : (int * int) list)];
  [%expect {|
  ((498 6) (498 5) (498 4))
  ((496 6) (497 6) (498 6)) |}]
;;
;;

Shared.Tuple.Map.add

let create_grid (parsed : (int * int) list list) =
  let res = Shared.Tuple.Map.empty in
  (* First level of folding, mark all points along a given path. *)
  let f accm ((x1, y1), (x2, y2)) =
    let points = all_points_in_between [ x1, y1 ] (x2, y2) in
    points
    |> List.fold ~init:accm ~f:(fun accm' x' ->
         Shared.map_add_overwrite ~key:x' ~data:Rock accm')
  in
  (* Second level of folding, mark all paths into the same map. *)
  let f' accm list_of_tuples_of_coords =
    list_of_tuples_of_coords |> List.fold ~init:accm ~f
  in
  parsed |> List.map ~f:Shared.list_into_pairs |> List.fold ~init:res ~f:f'
;;

let%expect_test "Grid input" =
  print_s
    [%sexp
      (small_input |> parse |> create_grid |> Map.to_alist : ((int * int) * elem) list)];
  [%expect
    {|
  (((494 9) Rock) ((495 9) Rock) ((496 6) Rock) ((496 9) Rock) ((497 6) Rock)
   ((497 9) Rock) ((498 4) Rock) ((498 5) Rock) ((498 6) Rock) ((498 9) Rock)
   ((499 9) Rock) ((500 9) Rock) ((501 9) Rock) ((502 4) Rock) ((502 5) Rock)
   ((502 6) Rock) ((502 7) Rock) ((502 8) Rock) ((502 9) Rock) ((503 4) Rock)) |}]
;;

let bounds grid =
  let coords = Map.keys grid in
  let min_x =
    coords
    |> List.map ~f:Tuple2.get1
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let max_x =
    coords
    |> List.map ~f:Tuple2.get1
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let min_y =
    coords
    |> List.map ~f:Tuple2.get2
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let max_y =
    coords
    |> List.map ~f:Tuple2.get2
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  min_x, max_x, min_y, max_y
;;

let display_grid grid =
  let min_x, max_x, _min_y, max_y = bounds grid in
  let screen =
    Array.init (max_y + 1) ~f:(fun _i ->
      Array.init (max_x - min_x + 1) ~f:(fun _i' -> Air))
  in
  grid
  |> Map.to_alist
  |> List.fold ~init:screen ~f:(fun accm ((x, y), v) ->
       accm.(y).(x - min_x) <- v;
       accm)
  |> Array.map ~f:(Array.map ~f:elem_to_char)
  |> Array.map ~f:(fun x -> x |> Array.to_list |> String.of_char_list)
  |> String.concat_array ~sep:"\n"
;;

let%expect_test "Grid input" =
  print_string (small_input |> parse |> create_grid |> display_grid);
  [%expect
    {|
  ..........
  ..........
  ..........
  ..........
  ....#...##
  ....#...#.
  ..###...#.
  ........#.
  ........#.
  #########. |}]
;;

(** Will return None when the sand falls into the abyss or Some updated grid with the sand marked.*)
let rec move_sand ?fl (x, y) grid =
  let _, _, _, max_y = bounds grid in
  let fl_val = Option.value fl ~default:max_y in
  if y >= fl_val
  then None
  else (
    let in_bounds (x, y) = x >= 0 && y >= 0 in
    let is_air (x, y) =
      (x, y) |> Map.find grid |> Option.value ~default:Air |> equal_elem Air
    in
    let next =
      [ x, y + 1; x - 1, y + 1; x + 1, y + 1 ]
      |> List.filter ~f:in_bounds
      |> List.filter ~f:is_air
    in
    match List.hd next with
    | Some (x', y') -> move_sand (x', y') grid ~fl:fl_val
    | None -> Some (Shared.map_add_overwrite ~key:(x, y) ~data:Sand grid))
;;

let%expect_test "Grid input" =
  let grid = small_input |> parse |> create_grid in
  let first_sand = move_sand (500, 0) grid in
  print_string (first_sand |> Option.value_exn |> display_grid);
  print_endline "\n\n==========\n";
  let all_sand =
    List.fold_until
      ~init:grid
      ~f:(fun accm x ->
        let grid' = move_sand x accm in
        match grid' with
        | None -> Stop accm
        | Some grid'' -> Continue grid'')
      ~finish:Fn.id
      (List.init 1_000_000 ~f:(fun _i -> 500, 0))
  in
  print_string (all_sand |> display_grid);
  [%expect
    {|
  ..........
  ..........
  ..........
  ..........
  ....#...##
  ....#...#.
  ..###...#.
  ........#.
  ......o.#.
  #########.

  ==========

  ..........
  ..........
  ......o...
  .....ooo..
  ....#ooo##
  ...o#ooo#.
  ..###ooo#.
  ....oooo#.
  .o.ooooo#.
  #########. |}]
;;

module Part1 = struct
  let solve' input =
    let grid = input |> parse |> create_grid in
    let all_sand =
      List.fold_until
        ~init:grid
        ~f:(fun accm x ->
          let grid' = move_sand x accm in
          match grid' with
          | None -> Stop accm
          | Some grid'' -> Continue grid'')
        ~finish:Fn.id
        (List.init 1_000_000 ~f:(fun _i -> 500, 0))
    in
    all_sand
  ;;

  let solve input =
    input |> solve' |> Map.data |> List.filter ~f:(equal_elem Sand) |> List.length
  ;;

  let%expect_test "Solve small input" =
    print_endline (solve' small_input |> display_grid : string);
    print_s [%sexp (solve small_input : int)];
    [%expect
      {|
      ..........
      ..........
      ......o...
      .....ooo..
      ....#ooo##
      ...o#ooo#.
      ..###ooo#.
      ....oooo#.
      .o.ooooo#.
      #########.
      24 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {|
      888 |}]
  ;;
end

module Part2 = struct
  (** Will not process a continue/stop decision. *)
  let rec move_sand ~fl (x, y) grid =
    (* let _, _, _, max_y = bounds grid in *)
    let fl_val = fl in
    let in_bounds (x, y) = x >= 0 && y >= 0 && y < fl_val in
    let is_air (x, y) =
      (x, y) |> Map.find grid |> Option.value ~default:Air |> equal_elem Air
    in
    let next =
      [ x, y + 1; x - 1, y + 1; x + 1, y + 1 ]
      |> List.filter ~f:in_bounds
      |> List.filter ~f:is_air
    in
    match List.hd next with
    | Some (x', y') -> move_sand (x', y') grid ~fl:fl_val
    | None -> Shared.map_add_overwrite ~key:(x, y) ~data:Sand grid
  ;;

  let solve' input =
    let grid = input |> parse |> create_grid in
    let _, _, _, max_y = bounds grid in
    let all_sand =
      List.fold_until
        ~init:grid
        ~f:(fun accm x ->
          let grid' = move_sand ~fl:(max_y + 2) x accm in
          match Map.find grid' (500, 0) with
          | None ->
            (* print_endline (grid' |> display_grid); *)
            Continue grid'
          | Some _ -> Stop grid')
        ~finish:Fn.id
        (List.init 1_000_000 ~f:(fun _i -> 500, 0))
    in
    all_sand
  ;;

  let solve input =
    input |> solve' |> Map.data |> List.filter ~f:(equal_elem Sand) |> List.length
  ;;

  let%expect_test "Solve small input" =
    print_endline (solve' small_input |> display_grid : string);
    print_s [%sexp (solve small_input : int)];
    [%expect
      {|
      ..........o..........
      .........ooo.........
      ........ooooo........
      .......ooooooo.......
      ......oo#ooo##o......
      .....ooo#ooo#ooo.....
      ....oo###ooo#oooo....
      ...oooo.oooo#ooooo...
      ..oooooooooo#oooooo..
      .ooo#########ooooooo.
      ooooo.......ooooooooo
      93 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {|
      26461 |}]
  ;;
end
