let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let%test _ = (last []) = None
let%test _ = (last [1]) = Some 1
let%test _ = (last [1; 2; 3]) = Some 3

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let rec at n = function
  | [] -> None
  | h :: t -> if n = 0 then Some h else at (n - 1) t

let length lst =
  let rec length' res = function
    | [] -> res
    | _ :: t -> length' (res + 1) t
  in
  length' 0 lst

let rev lst =
  let rec rev' res = function
  | [] -> res
  | h :: t -> rev' (h :: res) t
  in
  rev' [] lst

let palindrome lst =
  rev lst = lst

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec flatten' acc = function
    | [] -> acc
    | One x :: t -> flatten' (x :: acc) t
    | Many x :: t -> flatten' (flatten' acc x) t
  in
  lst |> flatten' [] |> rev

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | small -> small

let max lst =
  let rec aux n = function
    | [] -> n
    | h :: t -> if n > h then aux n t else aux h t
  in
  match lst with
  | [] -> None
  | h :: _ -> Some (aux h lst)

let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
    in
    rev (aux [] [] lst)

let encode lst =
  List.map (fun l -> (length l, List.hd l)) (pack lst)

let alt_encode lst =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
    if a = b then aux (count + 1) acc t
    else aux 0 ((count + 1, a) :: acc) t
  in
  rev (aux 0 [] lst)
