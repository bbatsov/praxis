open Util

let moves = lines "-"

let part1_strategy =
  [("A X", 4);
   ("A Y", 8);
   ("A Z", 3);
   ("B X", 1);
   ("B Y", 5);
   ("B Z", 9);
   ("C X", 7);
   ("C Y", 2);
   ("C Z", 6)]

let score strategy = moves |> List.map (fun move -> result (List.assoc_opt move strategy)) |> sum

let part2_strategy =
  [("A X", 3);
   ("A Y", 4);
   ("A Z", 8);
   ("B X", 1);
   ("B Y", 5);
   ("B Z", 9);
   ("C X", 2);
   ("C Y", 6);
   ("C Z", 7)]

let () =
  Printf.printf "Part1 score: %d\n" (score part1_strategy);
  Printf.printf "Part2 score: %d\n" (score part2_strategy)
