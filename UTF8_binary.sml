(* UTF-8 Encoding *)

val dollar = Char.ord #"$";
val cent = 162;
val euro = 8364;
val hwair = 66376;

fun binary_of_int n =
  case n of
    0 => []
  | _ => if (n mod 2) = 0
        then binary_of_int (n div 2) @ [#"0"]
        else binary_of_int (n div 2) @ [#"1"];

implode(binary_of_int dollar);
implode(binary_of_int cent);
implode(binary_of_int euro);
implode(binary_of_int hwair);

fun get_byte_count n =
  if n <= 127 then 1
  else if n<= 2047 then 2
  else if n<= 65535 then 3
  else 4;

get_byte_count dollar;
get_byte_count cent;
get_byte_count euro;
get_byte_count hwair;

fun binary_code_point n =
  if n<= 127 then
    let val b = binary_of_int n in
      let val l = length(b) in
        explode("abcd")
      end
    end
  else explode("0000");

format_binary dollar;
binary_code_point dollar;

fun binary_code_point2 n =
  let fun format f =
    case f of
      1 => explode("000000")
    | 2 => explode("00000")
    | 3 => explode("0000")
    | 4 => explode("000")
    | 5 => explode("00")
    | 6 => [#"0"]
    | 7 => []
    | 8 => explode("000")
    | 9 => explode("00")
    | 10 => [#"0"]
    | 11 => []
    | 12 => explode("0000")
    | 13 => explode("000")
    | 14 => explode("00")
    | 15 => [#"0"]
    | 16 => []
    | 17 => explode("000")
    | 18 => explode("00")
    | 19 => [#"0"]
    | 20 => []
    | _ => []
  in
    let val b = (binary_of_int n) in
      format (length b) @ b
    end
  end;

implode(binary_code_point2 dollar);
implode(binary_code_point2 cent);
implode(binary_code_point2 euro);
implode(binary_code_point2 hwair);


(*
let fun format f =
  case f of
    1 => explode("000000")
  | 2 => explode("00000")
  | 3 => explode("0000")
  | 4 => explode("000")
  | 5 => explode("00")
  | 6 => [#"0"]
  | 7 => []
  | 8 => explode("000")
  | 9 => explode("00")
  | 10 => [#"0"]
  | 11 => []
  | 12 => explode("0000")
  | 13 => explode("000")
  | 14 => explode("00")
  | 15 => [#"0"]
  | 16 => []
  | 17 => explode("000")
  | 18 => explode("00")
  | 19 => [#"0"]
  | 20 => []
in
  format l @ b
end
*)

(*
fun decode_utf8 c =
  let val byte_count = get_byte_count in
    let val binary = get_binary c in
      if byte_count = 1 then
*)
