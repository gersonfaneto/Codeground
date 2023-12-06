(* Day Three - Part I *)

open Core
module Set = Stdlib.Set

type symbol =
  { row : int
  ; column : int
  }

module Part = struct
  type t =
    { row : int
    ; column_start : int
    ; column_end : int
    ; value : int
    }
  [@@deriving ord]

  let collides part (symbol : symbol) =
    List.find Utils.directions ~f:(fun (x, y) ->
      let row = symbol.row + y in
      let column = symbol.column + x in
      part.row = row && part.column_end >= column && part.column_start <= column)
  ;;
end

module PartSet = Set.Make (Part)

let get_symbols_from_line line row =
  let chars = String.to_list line in
  List.filter_mapi chars ~f:(fun column value ->
    match value with
    | '0' .. '9' -> None
    | '.' -> None
    | _ -> Some { row; column })
;;

let get_parts_from_line line row =
  let chars = String.to_list line in
  let create_part start finish =
    let value = Int.of_string (String.slice line start (finish + 1)) in
    Part.{ row; column_start = start; column_end = finish; value }
  in
  let rec aux idx start finish chars acc =
    match start, finish, chars with
    | None, None, '0' .. '9' :: rest -> aux (idx + 1) (Some idx) (Some idx) rest acc
    | None, None, _ :: rest -> aux (idx + 1) None None rest acc
    | Some start, _, '0' .. '9' :: rest -> aux (idx + 1) (Some start) (Some idx) rest acc
    | Some start, Some finish, _ :: rest ->
      let part = create_part start finish in
      aux (idx + 1) None None rest (part :: acc)
    | Some start, _, [] ->
      let finish = String.length line - 1 in
      let part = create_part start finish in
      part :: acc
    | _, _, [] -> acc
    | _ -> assert false
  in
  aux 0 None None chars []
;;

let acc_from_lines lines getter =
  List.foldi lines ~init:[] ~f:(fun row acc line ->
    let symbols = getter line row in
    symbols @ acc)
;;

let () = Printf.printf "\nAdvent of Code - Day Three\n"

let () =
  let lines = Utils.read_lines "inputs/03.txt" in
  Printf.printf " Part I\n";
  let symbols = acc_from_lines lines get_symbols_from_line in
  let parts = acc_from_lines lines get_parts_from_line in
  let result =
    List.fold symbols ~init:PartSet.empty ~f:(fun acc symbol ->
      List.fold parts ~init:acc ~f:(fun set part ->
        match Part.collides part symbol with
        | Some _ -> PartSet.add part set
        | None -> set))
  in
  let result = PartSet.fold (fun part acc -> acc + part.value) result 0 in
  Printf.printf "   Answer: %d\n" result;
  assert true
;;
