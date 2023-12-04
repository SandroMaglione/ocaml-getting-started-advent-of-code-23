(* https://adventofcode.com/2023/day/1 *)
let input_test1 = "./input/day1/test1.txt"
let input_file = "./input/day1/input.txt"

type digits =
  | Empty
  | Full of int * int

let new_digits dig str =
  match dig, int_of_string_opt str with
  | Empty, None -> Empty
  | Empty, Some n -> Full (n, n)
  | Full (f, l), None -> Full (f, l)
  | Full (f, _), Some n -> Full (f, n)
;;

let dig_sum dig =
  match dig with
  | Empty -> 0
  | Full (f, l) -> (f * 10) + l
;;

let handle_line line =
  line
  |> String.fold_left (fun dig chr -> chr |> Char.escaped |> new_digits dig) Empty
  |> dig_sum
;;

let read_and_print file_to_read =
  file_to_read
  |> In_channel.open_text
  |> In_channel.input_lines
  |> List.map handle_line
  |> List.fold_left (fun sum n -> sum + n) 0
  |> string_of_int
  |> print_endline
;;

let main = read_and_print input_file
