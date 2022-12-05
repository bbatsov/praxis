open Util
open Printf

let input = lines "-"

module Move = struct
  type t = {src: int; dst: int; amt: int}

  let parse s =
    Scanf.sscanf s "move %d from %d to %d" (fun amt src dst ->
        {src= src - 1; dst= dst - 1; amt} )
end

let parse input =
  let (stacks, moves) = CCList.take_drop_while (fun l -> not (CCString.is_empty l)) input in
  let moves = List.tl moves in
  let moves = List.map Move.parse moves in
  let stacks = List.map CCString.to_list stacks in
  ((Array.of_list stacks), moves)

let print_char_list list =
  List.iter print_char list;
  print_newline ()

let print_stacks stacks =
  print_endline "--------------";
  Array.iter print_char_list stacks;
  print_endline "--------------"

let apply move stacks f =
  let Move.{src; dst; amt} = move in
  let top, bottom = stacks.(src) |> CCList.take_drop amt in
  let updated = (f top) @ stacks.(dst) in
  Array.set stacks src bottom;
  Array.set stacks dst updated

let solution stacks = Array.map List.hd stacks |> CCString.of_array |> String.uppercase_ascii

let () =
  (* part 1 *)
  let stacks, moves = parse input in
  List.iter (fun move -> apply move stacks List.rev) moves;
  printf "Part1: %s\n" (solution stacks);
  (* part 2 *)
  let (stacks, moves) = parse input in
  List.iter (fun move -> apply move stacks Fun.id) moves;
  printf "Part2: %s\n" (solution stacks)
