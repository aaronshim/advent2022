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

(** Sum type to keep trck of whether something is a file or a directory. *)
type entry =
  | Directory of string
  | File of int
[@@deriving sexp]

(** Reading a line that is a result of ls. See if it's a dir or not and then assign as the right enum. *)
let parse_directory_content str =
  if String.is_prefix ~prefix:"dir" str
  then str |> String.split_on_chars ~on:[ ' ' ] |> List.rev |> List.hd_exn |> Directory
  else str |> String.split_on_chars ~on:[ ' ' ] |> List.hd_exn |> Int.of_string |> File
;;

(** Flush the curr_dir_contents by writing it to the alist with the current working path as the key. *)
let write_dir_to_alist (path, curr_dir_contents, alist) =
  path, [], (path |> List.rev |> String.concat ~sep:"/", curr_dir_contents) :: alist
;;

(** Folding function for *)
let rec f (path, curr_dir_contents, alist) line =
  if String.is_prefix ~prefix:"$ " line
  then
    (* If we started a $ command chunk without proper initialization, go do that and run line again. *)
    if not (List.is_empty curr_dir_contents)
    then
      f (write_dir_to_alist (path, curr_dir_contents, alist)) line
      (* Find cd commands and either pop the path stack or continue down it. *)
    else if (* let _ = print_endline line in *)
            (* let _ = print_endline (path |> List.rev |> String.concat ~sep:"/") in *)
            String.is_prefix ~prefix:"$ cd " line
    then (
      (* dir that we are cd'ing to. *)
      let to_dir = line |> String.split_on_chars ~on:[ ' ' ] |> List.rev |> List.hd_exn in
      if String.equal to_dir ".."
      then List.tl_exn path, curr_dir_contents, alist
      else to_dir :: path, curr_dir_contents, alist)
    else
      (* It's probably a ls, so skip and move along to content reading mode *)
      path, curr_dir_contents, alist
  else
    (* If it's not a $ command, then keep reading directory contents. *)
    path, parse_directory_content line :: curr_dir_contents, alist
;;

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.fold ~init:([], [], []) ~f
  (* The last finished ls wouldn't have been added to the alist yet. *)
  |> write_dir_to_alist
  |> Tuple3.get3
  |> String.Map.of_alist_exn
;;

let%expect_test "Parse small input" =
  print_s [%sexp (parse small_input : entry list String.Map.t)];
  [%expect
    {|
    ((/ ((Directory d) (File 8504156) (File 14848514) (Directory a)))
     (//a ((File 62596) (File 2557) (File 29116) (Directory e)))
     (//a/e ((File 584)))
     (//d ((File 7214296) (File 5626152) (File 8033020) (File 4060174)))) |}]
;;

(** curr_dir is empty string if the entry path is an absolute path. *)
let rec size_of tree curr_dir entry =
  match entry with
  | File n -> n
  | Directory d ->
    let new_dir =
      if String.is_empty curr_dir then d else String.concat ~sep:"/" [ curr_dir; d ]
    in
    String.Map.find_exn tree new_dir
    |> List.map ~f:(size_of tree new_dir)
    |> Shared.sum_int
;;

let%expect_test "size_of" =
  print_s
    [%sexp
      ([ "//a/e"; "//a"; "//d"; "/" ]
       |> List.map ~f:(fun x -> Directory x)
       |> List.map ~f:(size_of (parse small_input) "")
        : int list)];
  [%expect {|
    (584 94853 24933642 48381165) |}]
;;

module Part1 = struct
  let solve input =
    let tree = input |> parse in
    let dirs = String.Map.keys tree in
    dirs
    |> List.map ~f:(fun x -> size_of tree "" (Directory x))
    |> List.filter ~f:(fun x -> x <= 100_000)
    |> Shared.sum_int
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int)];
    [%expect {| 95437 |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int)];
    [%expect {| 1449447 |}]
  ;;
end

module Part2 = struct
  let solve input =
    let tree = input |> parse in
    let dirs = String.Map.keys tree in
    let root_space = size_of tree "" (Directory "/") in
    let space_needed = 30_000_000 - (70_000_000 - root_space) in
    dirs
    |> List.map ~f:(fun x -> size_of tree "" (Directory x))
    |> List.filter ~f:(fun x -> x >= space_needed)
    |> List.min_elt ~compare:Int.compare
  ;;

  let%expect_test "Solve small input" =
    print_s [%sexp (solve small_input : int option)];
    [%expect {| (24933642) |}]
  ;;

  let%expect_test "Solve full input" =
    print_s [%sexp (solve full_input : int option)];
    [%expect {| (8679207) |}]
  ;;
end