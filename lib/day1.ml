(* https://adventofcode.com/2023/day/1 *)
let input_test1 = "./input/day1/test1.txt"
let input_test2 = "./input/day1/test2.txt"
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

let read_and_print1 file_to_read =
  file_to_read
  |> In_channel.open_text
  |> In_channel.input_lines
  |> List.map handle_line
  |> List.fold_left (fun sum n -> sum + n) 0
  |> string_of_int
;;

let main1 = read_and_print1 input_file

(* --- *)

type letters =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

let let_to_int ltt =
  match ltt with
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
;;

let sub_option line i len =
  try Some (String.sub line i len) with
  | Invalid_argument _ -> None
;;

let parse_n expect res line i =
  let l = String.length expect in
  match sub_option line i l with
  | Some str -> if String.equal str expect then Some (res, i + l) else None
  | _ -> None
;;

let parse_one = parse_n "one" One
let parse_two = parse_n "two" Two
let parse_three = parse_n "three" Three
let parse_four = parse_n "four" Four
let parse_five = parse_n "five" Five
let parse_six = parse_n "six" Six
let parse_seven = parse_n "seven" Seven
let parse_eight = parse_n "eight" Eight
let parse_nine = parse_n "nine" Nine

let parse_all line i =
  [ parse_one
  ; parse_two
  ; parse_three
  ; parse_four
  ; parse_five
  ; parse_six
  ; parse_seven
  ; parse_eight
  ; parse_nine
  ]
  |> List.map (fun prs -> prs line i)
;;

let rec from_char i line =
  if i >= String.length line
  then []
  else (
    let src = List.find_opt Option.is_some (parse_all line i) in
    match Option.join src with
    | Some (ltt, ni) -> ltt :: from_char ni line
    | None -> from_char (i + 1) line)
;;

let rec print_list = function
  | [] -> ""
  | ltt :: l -> string_of_int (let_to_int ltt) ^ " " ^ print_list l
;;

let read_and_print2 file_to_read =
  file_to_read
  |> In_channel.open_text
  |> In_channel.input_lines
  |> List.map (from_char 0)
  |> List.map print_list
  |> List.fold_left String.cat ""
;;

let main2 = read_and_print2 input_test2
