(* UTF-8 Encoding *)

(*few sample unicodes*)
val dollar  = "0024";   (*00100100*)
val cent    = "00A2";   (*11000010 10100010*)
val euro    = "20AC";   (*11100010 10000010 10101100*)
val hwair   = "10348";  (*11110000 10010000 10001101 10001000*)

fun getbits x =
  if x >= "0000" andalso  x <= "007F"         then "0xxxxxxx"
  else if x >= "0080" andalso  x <= "07FF"    then "110xxxxx10xxxxxx"
  else if x >= "0800" andalso  x <= "FFFF"    then "1110xxxx10xxxxxx10xxxxxx"
  else if x >= "10000" andalso  x <= "10FFFF" then "11110xxx10xxxxxx10xxxxxx10xxxxxx"
  else "-1";

getbits dollar;
getbits cent;
getbits euro;
getbits hwair;

length (explode dollar);
length (explode hwair);
