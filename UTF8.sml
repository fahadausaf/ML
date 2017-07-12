(* UTF-8 Encoding *)

fun string_length x = length(explode x);
string_length dollar;
string_length hwair;

(*few sample unicodes*)
val dollar  = "0024";   (*00100100*)
val cent    = "00A2";   (*11000010 10100010*)
val euro    = "20AC";   (*11100010 10000010 10101100*)
val hwair   = "10348";  (*11110000 10010000 10001101 10001000*)


fun get_number_of_bytes x =
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

get_number_of_bytes dollar;
get_number_of_bytes cent;
get_number_of_bytes euro;
get_number_of_bytes hwair;

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

get_binary(#"E");

fun hex_to_binary (l: char list): char list =
  case l of
    [] => []
  | x::xs => (get_binary x) @ (hex_to_binary xs);

hex_to_binary (explode euro);

(*
charToByte(#"A");

ord(#"0");
*)



















(**)
