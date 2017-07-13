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
    if byte_count = 4 then
      let val first = (explode "111100") @ List.take(binary, 2)
      and second = [#"1",#"0"] @ List.take(List.drop(binary,2), 6)
      and remaining = List.drop(binary,8)(*
      and third = [#"1",#"0"] @ List.take(remaining, 6)
      and fourth = [#"1",#"0"] @ List.drop(remaining, 6)*)
      in
        first @ second @ [#"1",#"0"] @ List.take(remaining, 6) @ [#"1",#"0"] @ List.drop(remaining, 6)
      end
    else if byte_count = 3 then
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
    else [#"0"] @ List.drop(binary, 9)
  end;

(* UTF-8 Decoding *)

fun get_hex s =
  case s of
    "0000" => "0"
  | "0001" => "1"
  | "0010" => "2"
  | "0011" => "3"
  | "0100" => "4"
  | "0101" => "5"
  | "0110" => "6"
  | "0111" => "7"
  | "1000" => "8"
  | "1001" => "9"
  | "1010" => "A"
  | "1011" => "B"
  | "1100" => "C"
  | "1101" => "D"
  | "1110" => "E"
  | "1111" => "F"
  | _ => "";

fun binary_to_hex(l: char list): string =
  case l of
    [] => ""
  | x1::x2::x3::x4::xs => get_hex((Char.toString x1) ^ (Char.toString x2) ^
    (Char.toString x3) ^ (Char.toString x4)) ^ (binary_to_hex xs)
  | _ => ""

fun decode_utf8 s =
  let val l = explode s in
    let val len = length(l) in
      if len = 32 then
        let val first = List.drop(List.take(l, 8), 6) in
          let val second = List.take(List.drop(l, 10), 6) in
            let val third = List.take(List.drop(l,18), 6) in
              let val fourth = List.drop(l, 26) in
                binary_to_hex(first @ second @ third @ fourth)
              end
            end
          end
        end
      else if len = 24 then
        let val first = List.drop(List.take(l, 8), 4) in
          let val second = List.take(List.drop(l, 10), 6) in
            let val third = List.take(List.drop(l,18), 6) in
              binary_to_hex(first @ second @ third)
            end
          end
        end
      else if len = 16 then
      let val first = List.drop(List.take(l, 8), 3) in
        let val second = List.take(List.drop(l, 10), 6) in
          binary_to_hex((explode "00000") @ first @ second)
        end
      end
      else binary_to_hex((explode "000000000") @ List.drop(l,1))
    end
  end;


(*few sample unicodes*)
val dollar  = "0024";   (*00100100*)
val cent    = "00A2";   (*11000010 10100010*)
val euro    = "20AC";   (*11100010 10000010 10101100*)
val hwair   = "10348";  (*11110000 10010000 10001101 10001000*)

implode(encode_utf8 dollar);
implode(encode_utf8 cent);
implode(encode_utf8 euro);
implode(encode_utf8 hwair);

decode_utf8 "11110000100100001000110110001000";
decode_utf8 (implode(encode_utf8 hwair));
decode_utf8 "111000101000001010101100";
decode_utf8 (implode(encode_utf8 euro));
decode_utf8 "1100001010100010";
decode_utf8 (implode(encode_utf8 cent));
decode_utf8 "00100100";
decode_utf8 (implode(encode_utf8 dollar));
