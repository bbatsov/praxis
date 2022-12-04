open Util
open Printf

let input = lines "-"

let transform_input lines =
  List.map (fun line -> Scanf.sscanf line "%d-%d,%d-%d" (fun a b c d -> (a, b), (c, d))) lines

let part1 =  transform_input input |> List.map (fun ((a, b), (c, d)) -> if ((a <= c && b >= d) || (c <= a && d >= b)) then 1 else 0) |> sum

let part2 = transform_input input |> List.map (fun ((a, b), (c, d)) -> if (Stdlib.max a c <= min b d) then 1 else 0) |> sum

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
