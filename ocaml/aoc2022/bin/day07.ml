open Util
open Printf

let input = lines "-"

let parse lines =
  let disk = Hashtbl.create 1000 in
  let rec aux lines path =
    match lines with
    | [] ->
        disk
    | "$ ls" :: tl ->
        aux tl path
    | line :: tl when String.starts_with line ~prefix:"dir" ->
        aux tl path
    | line :: tl when String.starts_with line ~prefix:"$ cd" -> (
        let name = Scanf.sscanf line "$ cd %s" Fun.id in
        match name with
        | ".." ->
            aux tl (List.tl path)
        | _ ->
            aux tl (name :: path) )
    | line :: tl ->
        let data, _ =
          Scanf.sscanf line "%d %s" (fun size name -> (size, name))
        in
        let _ =
          List.fold_right
            (fun comp acc ->
              let key = acc ^ comp in
              let _ =
                if Hashtbl.mem disk key then Hashtbl.replace disk key ((Hashtbl.find disk key) + data)
                else Hashtbl.add disk key data
              in
              key )
            path
            ""
        in
        aux tl path
  in
  aux lines []

let disk = parse input

let () =
  Hashtbl.iter (fun k v -> printf "%s: %d\n" k v) disk

let part1 disk =
  Hashtbl.fold (fun _key data acc -> if data <= 100000 then acc + data else acc) disk 0

let part2 disk =
  let max_space = 70000000 in
  let required_space = 30000000 in
  let used_space = Hashtbl.find disk "/" in
  let free_space = max_space - used_space in
  let needed_space = required_space - free_space in
  Hashtbl.fold
    (fun _ data acc ->
      if data >= needed_space && data < acc then data else acc )
    disk
    max_int

let () =
  printf "Part1: %d\n" (part1 disk);
  printf "part2: %d\n" (part2 disk)
