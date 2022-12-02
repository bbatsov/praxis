open Util

let input_lines = lines "-"

let all_dimensions = input_lines |> List.map (fun line -> line |> String.split_on_char 'x' |> List.map int_of_string)

let surface dimensions =
  match dimensions with
  | [l; w; h] ->
     let lw = l*w in
     let wh = w*h in
     let hl = h*l in
     2 * (lw + wh + hl) + List.hd (List.sort compare [lw; wh; hl])
  | _ -> 0

let ribbon dimensions =
  match dimensions with
  | [l; w; h] ->
     let lw = 2 * (l + w) in
     let wh = 2 * (w + h) in
     let hl = 2 * (h + l) in
     l * w * h + List.hd (List.sort compare [lw; wh; hl])
  | _ -> 0

let () =
  Printf.printf "2x3x4 = %d\n" (surface [2;3;4]);
  Printf.printf "1x1x10 = %d\n" (surface [1;1;10]);
  Printf.printf "Total: %d\n" (all_dimensions |> List.map surface |> sum);
  Printf.printf "Total ribbon: %d\n" (all_dimensions |> List.map ribbon |> sum)
