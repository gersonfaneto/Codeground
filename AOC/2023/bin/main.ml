(* Day One - Part I *)

let () =
  Printf.printf "Advent of Code - Day One\n";
;;

let () =
  let contents = Utils.read_lines "inputs/01-01.txt" in
  let result =
    List.fold_left
      (fun acc line ->
        let chars = String.to_seq line |> List.of_seq in
        let digits = List.filter Utils.is_digit chars in
        let number = Format.sprintf "%c%c" (List.hd digits) (List.hd @@ List.rev digits) in
        let number = int_of_string number in
        acc + number)
      0
      contents
  in
  Printf.printf "   Part I\n";
  Printf.printf "       Answer: %d\n" result
;;

(* Day One - Part II *)
