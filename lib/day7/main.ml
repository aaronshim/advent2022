open Core

let small_input =
  {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}
;;

let full_input = Stdio.In_channel.read_all "/workspace/advent2022/data/day7.txt"

(* In the format of (directory name, contents) *)
let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.group ~break:(fun x y ->
       (Char.equal (String.nget x 0) '$' || Char.equal (String.nget y 0) '$')
       && not (Char.equal (String.nget x 0) (String.nget y 0)))
  |> List.chunks_of ~length:2
  |> List.map ~f:(fun e ->
       ( List.nth_exn e 0
         |> List.rev
         |> List.tl_exn
         |> List.hd_exn
         |> String.split_on_chars ~on:[ ' ' ]
         |> List.rev
         |> List.hd_exn
       , List.nth_exn e 1 ))
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : (string * string list) list)];
  [%expect
    {|
    ((/ ("dir a" "14848514 b.txt" "8504156 c.dat" "dir d"))
     (a ("dir e" "29116 f" "2557 g" "62596 h.lst")) (e ("584 i"))
     (d ("4060174 j" "8033020 d.log" "5626152 d.ext" "7214296 k"))) |}]
;;

type entry =
  | Directory of string
  | File of int
[@@deriving sexp]

let parse_directory_content str =
  if String.is_prefix ~prefix:"dir" str
  then str |> String.split_on_chars ~on:[ ' ' ] |> List.rev |> List.hd_exn |> Directory
  else str |> String.split_on_chars ~on:[ ' ' ] |> List.hd_exn |> Int.of_string |> File
;;

let build_tree parsed =
  String.Map.of_alist_exn
    (List.map ~f:(fun (d, es) -> d, List.map ~f:parse_directory_content es) parsed)
;;

let%expect_test "build_tree" =
  print_s [%sexp (build_tree (parse small_input) : entry list String.Map.t)];
  [%expect
    {|
    ((/ ((Directory a) (File 14848514) (File 8504156) (Directory d)))
     (a ((Directory e) (File 29116) (File 2557) (File 62596)))
     (d ((File 4060174) (File 8033020) (File 5626152) (File 7214296)))
     (e ((File 584)))) |}]
;;

let rec size_of tree entry =
  match entry with
  | File n -> n
  | Directory d ->
    String.Map.find_exn tree d |> List.map ~f:(size_of tree) |> Shared.sum_int
;;

let%expect_test "size_of" =
  print_s
    [%sexp
      ([ "e"; "a"; "d"; "/" ]
       |> List.map ~f:(fun x -> Directory x)
       |> List.map ~f:(size_of (build_tree (parse small_input)))
        : int list)];
  [%expect {|
    (584 94853 24933642 48381165) |}]
;;

module Part1 = struct
  let solve input =
    let tree = input |> parse |> build_tree in
    let dirs = String.Map.keys tree in
    dirs
    |> List.map ~f:(fun x -> size_of tree (Directory x))
    |> List.filter ~f:(fun x -> x <= 100_000)
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 95437 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    ("Map.of_alist_exn: duplicate key" fct)
    Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
    Called from Base__Map.Tree0.Of_foldable.of_foldable_exn in file "src/map.ml", line 1268, characters 8-113
    Called from Base__Map.Using_comparator.of_alist_exn in file "src/map.ml", line 2381, characters 25-63
    Called from Day7__Main.Part1.solve in file "lib/day7/main.ml", line 106, characters 15-43
    Called from Day7__Main.Part1.(fun) in file "lib/day7/main.ml", line 120, characters 20-36
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
  ;;
end
