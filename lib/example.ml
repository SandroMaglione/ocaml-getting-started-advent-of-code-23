type digits =
  | Empty
  | Full of int * int

let digits_sum = function
  | Empty -> 0
  | Full (firstNum, lastNum) -> (firstNum * 10) + lastNum
;;

let collect_digits dig str =
  let int_option = int_of_string_opt str in
  match dig, int_option with
  | Empty, None -> Empty
  | Empty, Some n -> Full (n, n)
  | Full (f, l), None -> Full (f, l)
  | Full (f, _), Some n -> Full (f, n)
;;

let read_chars source =
  String.fold_left (fun dig chr -> collect_digits dig (Char.escaped chr)) Empty source
;;

let program source = source |> read_chars |> digits_sum
