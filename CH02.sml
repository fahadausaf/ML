(* VALUE DECLARATIONS *)
val seconds = 60;
val minutes = 60;
val hours = 24;
seconds*minutes*hours;
it div 24;
val secsinhour = it;
val secs_in_hour = seconds*minutes;

val pi = 3.14159;
val r = 2.0;
val area = pi * r * r;

fun area (r) = pi*r*r;
area(2.0);

area 1.0;

val pi = 0.0;

area(1.0);

(* NUMBERS, CHARACTER STRINGS AND TRUTH VALUES *)

fun square(x: real) = x*x;
fun square2 x : real = x*x;
fun square3 x = x*x : real;

"How now! a rat? Dead, for a ducat, dead!";
"Fair " ^ "Ophelia";
size (it);

fun title(name) = "The Duke of " ^ name;
title "York";
"This above all:\nto thine own self be true\n";

fun digit i = chr(i + ord #"0");
digit 49;

fun digit2 i = String.sub("abcdefghij", i);
digit2 5;
str (digit2 5);

fun sign n =
  if n>0 then 1
  else if n=0 then 0
  else (*n<0*) ~1;

fun isLower c = #"a" <= c andalso c <= #"z";
