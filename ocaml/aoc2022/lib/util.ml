let lines file =
  let contents = match file with
  | "-" -> In_channel.input_all In_channel.stdin
  | file -> In_channel.with_open_bin file In_channel.input_all
  in
  String.split_on_char '\n' contents

let max lst =
  let rec aux n = function
    | [] -> n
    | h :: t -> if n > h then aux n t else aux h t
  in
  match lst with
  | [] -> None
  | h :: _ -> Some (aux h lst)

let result opt =
  match opt with
  | (Some v) -> v
  | None -> 0

let first_three = function
  | [] | [ _ ] | [ _ ; _ ] -> []
  | x :: y :: z :: _ -> [x; y; z]