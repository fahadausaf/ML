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

(* PAIRS, TUPLES AND RECORDS *)

(2.5, ~1.2);

val zerovec = (0.0, 0.0);
val a = (1.5, 6.8);
val b = (3.6, 0.0);

fun lengthvec (x,y) = Math.sqrt(x*x + y*y);
lengthvec (1.0, 1.0);
lengthvec a;

fun negvec (x,y) : real*real = (~x, ~y);
negvec (1.0, 1.0);
val bn = negvec(b);

type vec = real*real;

fun average(x,y) = (x+y)/2.0;
average(3.1,3.3);

((2,0,3.5), zerovec);

fun addvec ((x1, y1), (x2, y2)) : vec = (x1+x2, y1+y2);

addvec((8.9, 4.4), b);
addvec(it, (0.1, 0.2));

fun subvec(v1,v2) = addvec(v1, negvec v2);

subvec(a,b);

fun distance(v1,v2) = lengthvec(subvec(v1,v2));

fun distance pairv = lengthvec(subvec pairv);

distance(a,b);

fun scalevec (r, (x,y)) : vec = (r*x, r*y);
scalevec(2.0, a);
scalevec(2.0, it);

val (xc,yc) = scalevec(4.0, a);
val ((x1,y1), (x2,y2)) = (addvec(a,b), subvec(a,b));

(* WRITING RECURSIVE FUNCTIONS *)

fun gcd(m,n) =
  if m=0 then n
  else gcd(n mod m, m);

gcd(5499,6812);

fun power(x,k) : real =
  if k=1 then x
  else if k mod 2 = 0 then power(x*x, k div 2)
  else x * power(x*x, k div 2);

power(2.0,10);
power(1.01,925);
Math.pow(1.01,925.0);

fun nextfib(prev, curr :int) = (curr, prev+curr);

nextfib(0,1);
nextfib it;
nextfib it;
nextfib it;

fun fibpair (n) = if n=1 then (0,1) else nextfib(fibpair(n-1));

fibpair 30;

fun itfib (n, prev, curr) : int =
  if n=1 then curr            (*does not work for n=0*)
  else itfib (n-1, curr, prev+curr);

fun fib(n) = itfib(n,0,1);
fib 30;

(* LOCAL DECLARATIONS *)

fun fraction (n,d) = (n div gcd(n,d), d div gcd(n,d));

fun divideboth (n,d, com: int) = (n div com, d div com);
fun fraction (n,d) = divideboth (n, d, gcd(n,d));

fun fraction (n,d) =
  let val com = gcd(n,d)
  in (n div com, d div com) end;

fun findroot (a, x, acc) =
  let val nextx = (a/x + x) / 2.0
  in if abs (x-nextx) < acc*x
    then nextx else findroot (a, nextx, acc)
  end;

fun sqroot a = findroot (a, 1.0, 1.0E~10);

sqroot 2.0;
it*it;


fun sqroot2 a =
  let val acc = 1.0E~10
    fun findroot x =
      let val nextx = (a/x + x) / 2.0
      in if abs (x-nextx) < acc*x
        then nextx else findroot nextx
      end
  in findroot 1.0 end;

sqroot2 3.0;
it*it;






















(**)
