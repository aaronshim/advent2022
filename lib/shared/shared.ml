open Core

let sum_int = List.fold ~init:0 ~f:( + )

let%expect_test "sum_int correctly" =
  print_s [%sexp (sum_int [ 1; 2; 3 ] : int)];
  [%expect {| 6 |}]
;;

let run_on_exactly_two f xs =
  match xs with
  | [ x; y ] -> Some (f x y)
  | _ -> None
;;

let%expect_test "run_on_exactly_two (two)" =
  print_s [%sexp (run_on_exactly_two ( + ) [ 1; 2 ] : int option)];
  [%expect {| (3) |}]
;;

let%expect_test "run_on_exactly_two (not two)" =
  print_s [%sexp (run_on_exactly_two ( + ) [ 1; 2; 3 ] : int option)];
  [%expect {| () |}]
;;

let string_to_chars s = List.init (String.length s) ~f:(String.get s)

let%expect_test "string_to_chars" =
  print_s [%sexp (string_to_chars "abc" : char list)];
  [%expect {| (a b c) |}]
;;

(* All ints in the string get returned. *)
let extract_ints str =
  str
  |> string_to_chars
  |> List.group ~break:(fun a b ->
       (Char.is_digit a && not (Char.is_digit b))
       || (Char.is_digit b && not (Char.is_digit a)))
  |> List.filter ~f:(List.for_all ~f:Char.is_digit)
  |> List.map ~f:(fun xs -> xs |> String.of_char_list |> Int.of_string)
;;

let%expect_test "extract_ints" =
  print_s
    [%sexp
      (List.map ~f:extract_ints [ "32;"; "1 + 2 = 3"; "Hello there 1 and 21" ]
        : int list list)];
  [%expect {| ((32) (1 2 3) (1 21)) |}]
;;

(** Functiorize int * int so that we can make a Set out of it. *)
module Tuple = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let remove_first_and_last xs = xs |> List.tl_exn |> List.rev |> List.tl_exn |> List.rev

let%expect_test "remove_first_and_last" =
  print_s [%sexp (remove_first_and_last [ 1; 2; 3; 4; 5 ] : int list)];
  [%expect {| (2 3 4) |}]
;;

(** [a; b; c] into [(a, b); (b, c)] so that we can run a map or some operation that takes two at a time. *)
let list_into_pairs xs =
  let unwrap pairs =
    match pairs with
    | Some (Second accm) -> accm
    | _ -> []
  in
  let into_pairs accm x =
    match accm with
    | None -> Some (First x)
    | Some (First x') -> Some (Second [ x', x ])
    | Some (Second accm') ->
      let _, x' = List.hd_exn accm' in
      Some (Second ((x', x) :: accm'))
  in
  xs |> List.fold ~init:None ~f:into_pairs |> unwrap |> List.rev
;;

let%expect_test "list_into_pairs" =
  print_s [%sexp (list_into_pairs [ 1; 2; 3; 4; 5 ] : (int * int) list)];
  [%expect {| ((1 2) (2 3) (3 4) (4 5)) |}]
;;

(** Default map-add will not overwrite. *)
let map_add_overwrite xs ~key ~data =
  match Map.add xs ~key ~data with
  | `Ok res -> res
  | `Duplicate ->
    let xs' = Map.remove xs key in
    Map.add_exn xs' ~key ~data
;;

let%expect_test "map_add_overwrite" =
  print_s
    [%sexp
      (Tuple.Map.of_alist_exn [ (1, 1), 2; (3, 3), 4 ]
       |> map_add_overwrite ~key:(1, 1) ~data:1000
       |> Map.to_alist
        : (Tuple.t * int) list)];
  [%expect {| (((1 1) 1000) ((3 3) 4)) |}]
;;