(* UTF-8 Encoding *)

val dollar = Char.ord #"$";
val cent = 162;
val euro = 8364;
val hwair = 66376;

fun get_binary n =
  case n of
    0 => []
  | _ => if (n mod 2) = 0
        then get_binary (n div 2) @ [#"0"]
        else get_binary (n div 2) @ [#"1"];

implode(get_binary dollar);
implode(get_binary cent);
implode(get_binary euro);
implode(get_binary hwair);
