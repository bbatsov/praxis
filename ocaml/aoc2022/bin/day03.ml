open Util
open Printf

let input = lines "-"

let split_string str =
  let len = String.length str in
  let mid = len / 2 in
  let part1 = (String.sub str 0 mid) in
  let part2 = (String.sub str mid mid) in
  (part1, part2)

let overlap str1 str2 =
  String.fold_left
    (fun res c ->
      if String.contains str2 c then c::res else res)
    [] str1

let overlap3 str1 str2 str3 =
  String.fold_left
    (fun res c ->
      if (String.contains str2 c) && (String.contains str3 c) then c::res else res)
    [] str1

let score ch =
  if (int_of_char ch >= 97) then (int_of_char ch) - 96
  else (int_of_char ch) - 38

let part1 = input |> List.map split_string |> List.map (fun pair -> let (s1, s2) = pair in (overlap s1 s2)) |> List.map List.hd |> List.map score |> sum

let part2 = input |> CCList.sublists_of_len 3 |> List.map (fun triplet -> let (s1 :: s2 :: s3 :: _) = triplet in (overlap3 s1 s2 s3)) |> List.map List.hd |> List.map score |> sum

let () = printf "Part1: %d\n" part1
let () = printf "Part2: %d\n" part2
