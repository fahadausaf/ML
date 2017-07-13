(* UTF-8 Encoding *)

fun get_byte_count x =
  let val l = explode x in
    if length(l) < 5 then
      let val first = List.nth(l, 0)
        and second = List.nth(l, 1)
        and third = List.nth(l, 2)
        and fourth = List.nth(l, 3) in
          if ord(first) = 48 andalso ord(second) = 48 andalso ord(third) < 55 then 1
          else if ord(first) = 48 andalso ord(second) >= 48 andalso ord(second) <= 55 then 2
          else 3
      end
    else 4
  end;

fun get_binary (c: char): char list =
  case c of
    #"0" => explode "0000"
  | #"1" => explode "0001"
  | #"2" => explode "0010"
  | #"3" => explode "0011"
  | #"4" => explode "0100"
  | #"5" => explode "0101"
  | #"6" => explode "0110"
  | #"7" => explode "0111"
  | #"8" => explode "1000"
  | #"9" => explode "1001"
  | #"A" => explode "1010"
  | #"B" => explode "1011"
  | #"C" => explode "1100"
  | #"D" => explode "1101"
  | #"E" => explode "1110"
  | #"F" => explode "1111"
  | _ => [];

fun hex_to_binary (l: char list): char list =
  case l of
    [] => []
  | x::xs => (get_binary x) @ (hex_to_binary xs);

fun encode_utf8 s =
let val byte_count = get_byte_count s
and binary = hex_to_binary (explode s) in
  if byte_count = 3 then
    let val first = (explode "1110") @ List.take(binary, 4)
    and remaining = List.drop(binary, 4)
    and second = [#"1",#"0"] @ List.take(remaining, 6)
    and third = [#"1",#"0"] @ List.drop(remaining, 6) in
      first @ second @ third
    end
  else if byte_count = 2 then
    let val first = (explode "110") @ List.drop(List.take(binary, 10), 5)
    and second = [#"1",#"0"] @ List.drop(binary, 10) in
      first @ second
    end
  else [#"1"]
  end;

(*few sample unicodes*)
val dollar  = "0024";   (*00100100*)
val cent    = "00A2";   (*11000010 10100010*)
val euro    = "20AC";   (*11100010 10000010 10101100*)
val hwair   = "10348";  (*11110000 10010000 10001101 10001000*)

implode(encode_utf8 dollar);
implode(encode_utf8 cent);
implode(encode_utf8 euro);
