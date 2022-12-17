open Core

let small_input = {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|}

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day12.txt"

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.map ~f:Shared.string_to_chars
  |> List.map ~f:Array.of_list
  |> Array.of_list
;;

let rec elevation_of c =
  match c with
  | 'S' -> elevation_of 'a'
  | 'E' -> elevation_of 'z'
  | _ -> Char.to_int c - Char.to_int 'a'
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : char array array)];
  [%expect
    {|
    ((S a b q p o n m) (a b c r y x x l) (a c c s z E x k) (a c c t u v w j)
     (a b d e f g h i)) |}]
;;

let in_bounds (grid : 'a array array) ((x, y) : int * int) : bool =
  x >= 0 && x < Array.length grid && y >= 0 && y < Array.length grid.(0)
;;

(** Functiorize int * int so that we can make a Set out of it. *)
module Tuple = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let rec bfs targets grid visited q ~up =
  (* print_s [%sexp (q : (int * int * int) Queue.t)]; *)
  if Queue.length q = 0
  then None
  else (
    let x, y, n = Queue.dequeue_exn q in
    (* let visited' = Set.add visited (x, y) in *)
    if targets |> List.map ~f:(Char.equal grid.(x).(y)) |> List.reduce_exn ~f:( || )
    then Some n
    else (
      let curr_elevation = elevation_of grid.(x).(y) in
      let next =
        [ x + 1, y; x, y + 1; x - 1, y; x, y - 1 ]
        |> List.filter ~f:(in_bounds grid)
        |> List.filter ~f:(fun (x', y') ->
             (not (Set.mem visited (x', y'))) (* not visited *)
             (* && elevation_of grid.(x').(y')
                >= curr_elevation  *)
             (* not backtracking down the mountain *)
             &&
             if up
             then elevation_of grid.(x').(y') - curr_elevation <= 1
             else curr_elevation - elevation_of grid.(x').(y') <= 1)
        (* at most 1 up in elevation *)
      in
      (* Gotta add to the visited set here, or we might queue the same coordinates over and over. *)
      let visited' = List.fold ~init:visited ~f:Set.add next in
      let next' = next |> List.map ~f:(fun (a, b) -> a, b, n + 1) in
      Queue.enqueue_all q next';
      bfs targets grid visited' q ~up))
;;

let%expect_test "Parse small input" =
  let grid = parse small_input in
  print_s
    [%sexp
      (bfs ~up:true [ 'E' ] grid Tuple.Set.empty (Queue.of_list [ 0, 0, 0 ]) : int option)];
  [%expect {|
    (31) |}]
;;

let find_in_grid target (grid : char array array) =
  Array.find_mapi
    ~f:(fun r xs ->
      match
        Array.find_mapi ~f:(fun c x -> if Char.equal x target then Some c else None) xs
      with
      | Some c -> Some (r, c)
      | None -> None)
    grid
;;

module Part1 = struct
  let solve input =
    let grid = parse input in
    match find_in_grid 'S' grid with
    | Some (x, y) -> bfs ~up:true [ 'E' ] grid Tuple.Set.empty (Queue.of_list [ x, y, 0 ])
    | None -> None
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int option)];
    [%expect {|
      (31) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {|
      (437) |}]
  ;;
end

module Part2 = struct
  let solve input =
    let grid = parse input in
    match find_in_grid 'E' grid with
    | Some (x, y) ->
      bfs ~up:false [ 'S'; 'a' ] grid Tuple.Set.empty (Queue.of_list [ x, y, 0 ])
    | None -> None
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int option)];
    [%expect {|
      (29) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {|
      (430) |}]
  ;;
end
