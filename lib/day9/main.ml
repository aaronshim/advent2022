open Core

let small_input = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day9.txt"

type direction =
  | Up of int
  | Down of int
  | Left of int
  | Right of int
[@@deriving sexp]

let parse_line line =
  match String.split_on_chars ~on:[ ' ' ] line with
  | [ "U"; n ] -> Some (Up (Int.of_string n))
  | [ "D"; n ] -> Some (Down (Int.of_string n))
  | [ "L"; n ] -> Some (Left (Int.of_string n))
  | [ "R"; n ] -> Some (Right (Int.of_string n))
  | _ -> None
;;

let parse input =
  input |> String.split_lines |> List.map ~f:String.strip |> List.filter_map ~f:parse_line
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : direction list)];
  [%expect
    {|
    ((Right 4) (Up 4) (Left 3) (Down 1) (Right 4) (Down 1) (Left 5) (Right 2)) |}]
;;

let is_done_moving_in_direction d =
  match d with
  | Up n -> n = 0
  | Down n -> n = 0
  | Left n -> n = 0
  | Right n -> n = 0
;;

(** Only move the tail if it is not adjacent. *)
let is_tail_ok (x_h, y_h) (x_t, y_t) = abs (x_h - x_t) <= 1 && abs (y_h - y_t) <= 1

let%expect_test "is_tail_ok" =
  print_s
    [%sexp
      (List.map
         ~f:(fun (e1, e2) -> is_tail_ok e1 e2)
         [ (1, 1), (2, 1); (1, 2), (2, 1); (1, 1), (1, 1) ]
        : bool list)];
  [%expect {| (true true true) |}]
;;

(** Move either one square in one direction or one square in both directions. *)
let move_tail (x_h, y_h) (x_t, y_t) =
  (* Forcing this check to happen here so that we don't move inadvertently. *)
  if is_tail_ok (x_h, y_h) (x_t, y_t)
  then x_t, y_t
  else (
    let x_t' =
      if x_h - x_t > 0 then x_t + 1 else if x_h - x_t < 0 then x_t - 1 else x_t
    in
    let y_t' =
      if y_h - y_t > 0 then y_t + 1 else if y_h - y_t < 0 then y_t - 1 else y_t
    in
    x_t', y_t')
;;

let%expect_test "move_tail" =
  print_s
    [%sexp
      (List.map
         ~f:(fun (e1, e2) -> move_tail e1 e2)
         [ (3, 1), (1, 1); (1, 1), (1, 3); (2, 3), (1, 1); (3, 2), (1, 1) ]
        : (int * int) list)];
  [%expect {| ((2 1) (1 2) (2 2) (2 2)) |}]
;;

module Part1 = struct
  (** For folding. *)
  let rec f ((x_h, y_h), (x_t, y_t), visited) direction =
    if is_done_moving_in_direction direction
    then (x_h, y_h), (x_t, y_t), visited
    else (
      let x_h', y_h', direction' =
        match direction with
        | Up n -> x_h, y_h + 1, Up (n - 1)
        | Down n -> x_h, y_h - 1, Down (n - 1)
        | Left n -> x_h - 1, y_h, Left (n - 1)
        | Right n -> x_h + 1, y_h, Right (n - 1)
      in
      let x_t', y_t', visited' =
        (* Doing check here even if it's done in move_tail so that we know whether to update the visited set. *)
        if is_tail_ok (x_h', y_h') (x_t, y_t)
        then x_t, y_t, visited
        else (
          let x_t'', y_t'' = move_tail (x_h', y_h') (x_t, y_t) in
          ( x_t''
          , y_t''
          , String.Set.add visited (Sexp.to_string [%sexp (x_t'', y_t'' : int * int)]) ))
      in
      f ((x_h', y_h'), (x_t', y_t'), visited') direction')
  ;;

  let solve' input =
    input
    |> parse
    |> List.fold ~init:((0, 0), (0, 0), String.Set.of_list [ "(0 0)" ]) ~f
    |> Tuple3.get3
  ;;

  let solve input = input |> solve' |> Set.length

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : String.Set.t)];
    [%expect
      {|
      ("(0 0)" "(1 0)" "(1 2)" "(2 0)" "(2 2)" "(2 4)" "(3 0)" "(3 2)" "(3 3)"
       "(3 4)" "(4 1)" "(4 2)" "(4 3)") |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {|
      13 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 6563 |}]
  ;;
end

module Part2 = struct
  let ten_on_origin = List.init 10 ~f:(fun _ -> 0, 0)
  let set_with_origin = String.Set.of_list [ "(0 0)" ]

  let rec visualize' (coords : (int * int) list) (i : int) grid =
    match coords with
    | (x, y) :: rest ->
      (* let _ = print_endline (Int.to_string x) in *)
      (* let _ = print_endline (Int.to_string y) in *)
      (* let _ = print_s [%sexp (grid.(y) : string array)] in *)
      let max_y = Array.length grid in
      grid.(max_y - y - 1).(x) <- Int.to_string i;
      visualize' rest (i + 1) grid
    | [] ->
      grid |> Array.map ~f:(String.concat_array ~sep:" ") |> String.concat_array ~sep:"\n"
  ;;

  let visualize coords =
    let max_x =
      Option.value_exn
        (List.max_elt (List.map ~f:Tuple2.get1 coords) ~compare:Int.compare)
    in
    let max_y =
      Option.value_exn
        (List.max_elt (List.map ~f:Tuple2.get2 coords) ~compare:Int.compare)
    in
    (* let _ = print_endline (Int.to_string max_x) in *)
    (* let _ = print_endline (Int.to_string max_y) in *)
    let grid =
      Array.of_list
        (List.init
           ((max_y + 1) * 2)
           ~f:(fun _ -> Array.init ((max_x + 1) * 2) ~f:(fun _ -> "*")))
    in
    (* print_s [%sexp (grid : string array array)]; *)
    visualize' coords 0 grid
  ;;

  let%expect_test "Visualize" =
    print_string (visualize [ 0, 0; 0, 1; 2, 3 ]);
    [%expect
      {|
    * * * * * *
    * * * * * *
    * * * * * *
    * * * * * *
    * * 2 * * *
    * * * * * *
    1 * * * * *
    0 * * * * * |}]
  ;;

  (** Update all the old coords into new coords by following the last updated one, but in reverse order. *)
  let rec follow_the_leader
    (new_coords : (int * int) list)
    (old_coords : (int * int) list)
    : (int * int) list
    =
    match List.hd old_coords with
    | None -> new_coords
    | Some next_coord_to_update ->
      let last_updated_coord = List.hd_exn new_coords in
      let just_updated = move_tail last_updated_coord next_coord_to_update in
      (* let _ = print_endline "Updating..." in
      let _ = print_s [%sexp (next_coord_to_update : int * int)] in
      let _ = print_endline "To follow..." in
      let _ = print_s [%sexp (last_updated_coord : int * int)] in
      let _ = print_endline "Result..." in
      let _ = print_s [%sexp (just_updated : int * int)] in *)
      follow_the_leader (just_updated :: new_coords) (List.tl_exn old_coords)
  ;;

  (** For folding. *)
  let rec f (coords_list, visited) direction =
    if is_done_moving_in_direction direction
    then (* let _ = print_endline (visualize coords_list) in *)
      coords_list, visited
    else (
      let x_h, y_h = List.hd_exn coords_list in
      let x_h', y_h', direction' =
        match direction with
        | Up n -> x_h, y_h + 1, Up (n - 1)
        | Down n -> x_h, y_h - 1, Down (n - 1)
        | Left n -> x_h - 1, y_h, Left (n - 1)
        | Right n -> x_h + 1, y_h, Right (n - 1)
      in
      let new_coords_reversed =
        follow_the_leader [ x_h', y_h' ] (List.tl_exn coords_list)
      in
      let x_t', y_t' = List.hd_exn new_coords_reversed in
      let visited' =
        String.Set.add visited (Sexp.to_string [%sexp (x_t', y_t' : int * int)])
      in
      let new_coords = List.rev new_coords_reversed in
      f (new_coords, visited') direction')
  ;;

  let%expect_test "Visualize (intermediate)" =
    let coords, visited = f (ten_on_origin, set_with_origin) (Right 4) in
    print_endline "==========";
    print_s [%sexp (Right 4 : direction)];
    print_endline "==========";
    print_s [%sexp (visited : String.Set.t)];
    print_endline "==========";
    print_string (visualize coords);
    print_endline "";
    let coords2, visited2 = f (coords, visited) (Up 4) in
    print_endline "==========";
    print_s [%sexp (Up 4 : direction)];
    print_endline "==========";
    print_s [%sexp (visited2 : String.Set.t)];
    print_endline "==========";
    print_string (visualize coords2);
    let coords3, visited3 = f (coords2, visited2) (Left 3) in
    print_endline "==========";
    print_s [%sexp (Left 3 : direction)];
    print_endline "==========";
    print_s [%sexp (visited3 : String.Set.t)];
    print_endline "==========";
    print_string (visualize coords3);
    [%expect
      {|
    ==========
    (Right 4)
    ==========
    ("(0 0)")
    ==========
    * * * * * * * * * *
    9 3 2 1 0 * * * * *
    ==========
    (Up 4)
    ==========
    ("(0 0)")
    ==========
    * * * * * * * * * *
    * * * * * * * * * *
    * * * * * * * * * *
    * * * * * * * * * *
    * * * * * * * * * *
    * * * * 0 * * * * *
    * * * * 1 * * * * *
    * * 4 3 2 * * * * *
    * 5 * * * * * * * *
    9 * * * * * * * * *==========
    (Left 3)
    ==========
    ("(0 0)")
    ==========
    * * * * * * * *
    * * * * * * * *
    * * * * * * * *
    * * * * * * * *
    * * * * * * * *
    * 0 1 * * * * *
    * * * 2 * * * *
    * * 4 3 * * * *
    * 5 * * * * * *
    9 * * * * * * * |}]
  ;;

  let solve' input =
    input |> parse |> List.fold ~init:(ten_on_origin, set_with_origin) ~f |> Tuple2.get2
  ;;

  let solve input = input |> solve' |> Set.length

  let%expect_test "Solve small input" =
    print_s [%sexp (solve' small_input : String.Set.t)];
    [%expect {|
      ("(0 0)") |}]
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {|
      1 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 2653 |}]
  ;;
end
