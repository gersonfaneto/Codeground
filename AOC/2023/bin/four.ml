(* Day Four - Part I *)

open Core
module Set = Stdlib.Set

module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

let get_numbers_from_string string =
  String.strip string
  |> String.split ~on:' '
  |> List.filter_map ~f:(fun num ->
    match String.length num with
    | x when x > 0 -> Some (Int.of_string num)
    | _ -> None)
  |> IntSet.of_list
;;

let get_game_from_line line =
  let game = String.split line ~on:':' |> List.last_exn |> String.strip in
  let winning_numbers = String.split game ~on:'|' |> List.hd_exn |> get_numbers_from_string in
  let our_numbers = String.split game ~on:'|' |> List.last_exn |> get_numbers_from_string in
  winning_numbers, our_numbers
;;

let () = Printf.printf "\nAdvent of Code - Day Four\n"

let () =
  let lines = Utils.read_lines "input.txt" in
  Printf.printf " Part I\n";
  List.fold lines ~init:0 ~f:(fun acc line ->
    let winning_numbers, our_numbers = get_game_from_line line in
    let matches = IntSet.inter winning_numbers our_numbers |> IntSet.cardinal in
    acc + Int.shift_left 1 (matches - 1))
  |> Printf.printf "       Answer: %d\n"
;;

(* Day Four - Part II *)

let () =
  let lines = Utils.read_lines "input.txt" in
  let extras = Array.create ~len:(List.length lines) 1 in
  Printf.printf " Part I\n";
  List.iteri lines ~f:(fun idx line ->
    let winning_numbers, our_numbers = get_game_from_line line in
    let matches = IntSet.inter winning_numbers our_numbers |> IntSet.cardinal in
    for i = 0 to matches - 1 do
      extras.(idx + i + 1) <- extras.(idx + i + 1) + extras.(idx)
    done);
  Array.fold extras ~init:0 ~f:(fun acc x -> acc + x) |> Printf.printf "       Answer: %d\n"
;;
