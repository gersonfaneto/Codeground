(* Day One - Part I *)

open Core

let () = Printf.printf "\nAdvent of Code - Day One\n"

let () =
  let lines = Utils.read_lines "inputs/01.txt" in
  Printf.printf "   Part I\n";
  List.fold lines ~init:0 ~f:(fun acc line ->
    let chars = String.to_list line in
    let digits = List.filter chars ~f:Utils.is_digit in
    let number = Format.sprintf "%c%c" (List.hd_exn digits) (List.last_exn digits) in
    let number = int_of_string number in
    acc + number)
  |> Printf.printf "       Answer: %d\n"
;;

(* Day One - Part II *)

let literals =
  [ "one", "1"
  ; "two", "2"
  ; "three", "3"
  ; "four", "4"
  ; "five", "5"
  ; "six", "6"
  ; "seven", "7"
  ; "eight", "8"
  ; "nine", "9"
  ; "1", "1"
  ; "2", "2"
  ; "3", "3"
  ; "4", "4"
  ; "5", "5"
  ; "6", "6"
  ; "7", "7"
  ; "8", "8"
  ; "9", "9"
  ]
;;

let map_to_digit str pos =
  List.find_map literals ~f:(fun (literal, value) ->
    match String.substr_index ~pos str ~pattern:literal with
    | Some matched when matched = pos -> Some value
    | _ -> None)
;;

let str_to_numbers str =
  let map_to_number = map_to_digit str in
  List.range 0 (String.length str) |> List.filter_map ~f:map_to_number
;;

let () =
  let lines = Utils.read_lines "inputs/01.txt" in
  Printf.printf "   Part II\n";
  List.fold lines ~init:0 ~f:(fun acc line ->
    let numbers = str_to_numbers line in
    let number = Printf.sprintf "%s%s" (List.hd_exn numbers) (List.last_exn numbers) in
    let number = Int.of_string number in
    acc + number)
  |> Printf.printf "       Answer: %d\n"
;;
