open Util
open Printf

let input = contents "-"

let unique chars =
  let l1 = List.length chars in
  let l2 = CCList.uniq ~eq:( = ) chars |> List.length in
  l1 = l2

let part1 msg =
  let chars = CCString.to_list msg in
  let rec aux pos = function
    | a :: b :: c :: d :: tail ->
       if (unique [a;b;c;d]) then pos + 4
       else aux (pos + 1) (b :: c :: d :: tail)
    | _ -> -1 in
  aux 0 chars

(* basically a more generic version of part1 *)
let part2 msg size =
  let chars = CCString.to_list msg in
  let sublists = CCList.sublists_of_len size ~offset:1 chars in
  match CCList.find_idx unique sublists with
  | Some (idx, _) -> idx + size
  | None -> -1

let () =
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input 14)
