(* UTF-8 Encoding *)

fun binary_of_int n = case n of
    0 => []
  | _ => if (n mod 2) = 0
        then binary_of_int (n div 2) @ [#"0"]
        else binary_of_int (n div 2) @ [#"1"];

fun binary_code_point n =
  let fun format f = case f of
      1 => explode("000000") | 2 => explode("00000") | 3 => explode("0000")
    | 4 => explode("000") | 5 => explode("00") | 6 => [#"0"] | 7 => []
    | 8 => explode("000") | 9 => explode("00") | 10 => [#"0"] | 11 => []
    | 12 => explode("0000") | 13 => explode("000") | 14 => explode("00")
    | 15 => [#"0"] | 16 => [] | 17 => explode("000") | 18 => explode("00")
    | 19 => [#"0"] | 20 => [] | _ => []
  in
    let val b = (binary_of_int n) in
      format (length b) @ b
    end
  end;

fun encode_utf8 n =
  let val code_point = binary_code_point n in
    if n < 127 then [#"0"] @ code_point
    else if n <= 2047 then
      (explode "110") @ List.take(code_point, 5) @ [#"1",#"0"] @ List.drop(code_point, 5)
    else if n <= 65535 then
      (explode "1110") @ List.take(code_point, 4) @ [#"1",#"0"] @
      List.take(List.drop(code_point, 4), 6) @ [#"1",#"0"] @ List.drop(code_point, 10)
    else (explode "111100") @ List.take(code_point, 2) @ [#"1",#"0"] @ List.take(List.drop(code_point,2), 6)
    @ [#"1",#"0"] @ List.take(List.drop(code_point,8), 6) @ [#"1",#"0"] @ List.drop(code_point, 14)
  end;

val dollar = Char.ord #"$";
val cent = 162;
val euro = 8364;
val hwair = 66376;

implode(encode_utf8 hwair);

(*
implode(binary_of_int dollar);
implode(binary_of_int cent);
implode(binary_of_int euro);
implode(binary_of_int hwair);

get_byte_count dollar;
get_byte_count cent;
get_byte_count euro;
get_byte_count hwair;

implode(binary_code_point dollar);
implode(binary_code_point cent);
implode(binary_code_point euro);
implode(binary_code_point hwair);
*)
