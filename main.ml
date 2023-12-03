(* https://adventofcode.com/2023/day/1 *)

type digits =
  | Empty
  | Full of int * int


let new_digits dig str = match (dig, int_of_string_opt str) with
  | (Empty, None) -> Empty
  | (Empty, Some n) -> Full (n, n)
  | (Full (f, l), None) -> Full (f, l)
  | (Full (f, l), Some n) -> Full (f, n)

let dig_sum dig = match dig with
  | Empty -> 0
  | Full (f, l) -> f * 10 + l

let handle_line line = line
  |> String.fold_left (fun dig -> fun chr -> chr |> Char.escaped |> new_digits dig) Empty
  |> dig_sum

let read_and_print file_to_read = file_to_read
  |> In_channel.open_text
  |> In_channel.input_lines
  |> List.map handle_line
  |> List.fold_left (fun sum -> fun n -> sum + n) 0
  |> string_of_int
  |> print_endline


let () = read_and_print "aoc1.txt";
