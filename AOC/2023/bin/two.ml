(* Day Two - Part I *)

open Core

let () = Printf.printf "\nAdvent of Code - Day Two\n"

let () =
  let lines = Utils.read_lines "inputs/02-01.txt" in
  Printf.printf " Part I\n";
  List.foldi lines ~init:0 ~f:(fun idx acc line ->
    let game = String.split line ~on:':' |> List.last_exn |> String.strip in
    let sets = String.split game ~on:';' |> List.map ~f:String.strip in
    let pairs =
      List.fold sets ~init:[] ~f:(fun acc set ->
        let pairs = String.split set ~on:',' |> List.map ~f:String.strip in
        acc @ pairs)
    in
    let matches =
      List.filter pairs ~f:(fun pair ->
        let color = String.split pair ~on:' ' |> List.last_exn |> String.strip in
        let value = String.split pair ~on:' ' |> List.hd_exn |> String.strip in
        let value = int_of_string value in
        match color, value with
        | "red", value when value > 12 -> true
        | "green", value when value > 13 -> true
        | "blue", value when value > 14 -> true
        | _ -> false)
    in
    if List.length matches = 0 then acc + (idx + 1) else acc + 0)
  |> Printf.printf "       Answer: %d\n"
;;

(* Day Two - Part II *)
