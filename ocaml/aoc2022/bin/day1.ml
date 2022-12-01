open Util

let input_lines = lines "-"

let calories_per_elf lines =
  let rec aux lines sum calories =
    match lines with
    | [] -> sum :: calories
    | h :: t -> if h = "" then aux t 0 (sum :: calories)
                else aux t (sum + int_of_string h) calories
  in
  aux lines 0 []

let max_calories = calories_per_elf input_lines |> max |> result

let () = Printf.printf "Max calories per elf: %d\n" max_calories

let top_calories = calories_per_elf input_lines |> List.sort compare |> List.rev |> first_three |> List.fold_left ( + ) 0

let () = Printf.printf "Sum of top 3 calories per elf: %d\n" top_calories
