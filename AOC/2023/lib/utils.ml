let read_lines filename =
  let channel = open_in filename in
  let rec aux acc =
    match input_line channel with
    | line -> aux (line :: acc)
    | exception End_of_file ->
      close_in channel;
      List.rev acc
  in
  aux []
;;

let print_list list =
  List.iter (fun x -> print_endline x) list

let reverse_string str =
  let rec aux idx =
    match idx with
    | 0 -> Char.escaped str.[0]
    | _ -> Char.escaped str.[idx] ^ aux (idx - 1)
  in
  aux (String.length str - 1)
;;

let is_digit char =
  match char with
  | '0' .. '9' -> true
  | _ -> false
;;
