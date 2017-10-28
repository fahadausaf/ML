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

chr(65);
chr(66);
chr(67);
chr(49 + 48);
ord(#"A");
ord(#"B");
ord(#"0");

fun digit i = chr(i + ord #"0");
digit 49;

fun digit2 i = String.sub("abcdefghij", i);
digit2 5;
str (digit2 5);

fun sign n =
  if n>0 then 1
  else if n=0 then 0
  else (*n<0*) ~1;

sign 5;
sign 0;
sign 4-8;
sign ~4;

fun isLower c = #"a" <= c andalso c <= #"z";

isLower #"c";
isLower #"H";

(*
Exercise 2.5 Let d be an integer and m a string. Write an ML boolean expression
that is true just when d and m form a valid date: say 25 and "October".
Assume it is not a leap year.
*)

fun validDate(d,m) =
1<=d andalso
(m = "January"   andalso d<=31  orelse
 m = "February"  andalso d<=28  orelse
 m = "March"     andalso d<=31  orelse
 m = "April"     andalso d<=30  orelse
 m = "May"       andalso d<=31  orelse
 m = "June"      andalso d<=30  orelse
 m = "July"      andalso d<=31  orelse
 m = "August"    andalso d<=31  orelse
 m = "September" andalso d<=30  orelse
 m = "October"   andalso d<=31  orelse
 m = "November"  andalso d<=30  orelse
 m = "December"  andalso d<=31);

validDate (25,"February");
validDate (28,"February");
validDate (30,"February");


(* PAIRS, TUPLES AND RECORDS *)

(2.5, ~1.2);

val zerovec = (0.0, 0.0);
val a = (1.5, 6.8);
val b = (3.6, 0.9);

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

(*  Exercise 2.6 Write a function to determine whether one time of day, in the
    form (hours, minutes, AM or PM), comes before another. As an example,
    (11, 59, "AM") comes before (1, 15, "PM"). *)

fun earlier ((h1, m1, apm1), (h2:int, m2:int, apm2)) =
  apm1 = "AM" andalso apm2 = "PM"
  orelse apm1=apm2 andalso (h1<h2 orelse h1=h2 andalso m1<m2);

earlier ((12,12,"AM"),(12,12,"PM"));

(*  Exercise 2.7 Old English money had 12 pence in a shilling and 20 shillings
    in a pound. Write functions to add and subtract two amounts, working with
    triples (pounds, shillings, pence). *)

fun quorem (m,n) = (m div n, m mod n);
fun addPounds (l1, l2, (scarry, s), d) = (scarry + l1 + l2, s, d);
fun addShillings (l1, l2, s1, s2, (dcarry,d)) =
  addPounds (l1, l2, quorem(dcarry + s1 + s2, 20), d);
fun addMoney ((l1,s1,d1), (l2,s2,d2)) =
  addShillings (l1, l2, s1, s2, quorem(d1 + d2, 12));

{name="Jones", age=25, salary=15300};
{name="Jones", salary=15300, age=25};

val henryV =
  { name    = "Henry V",
    born    = 1387,
    crowned = 1412,
    died    = 1422,
    quote   = "Bid them achieve me and then sell my bones" };

val henryVI =
  { name    = "Henry VI",
    born    = 1421,
    crowned = 1422,
    died    = 1471,
    quote   = "weep, wretched man, \
    \ I'll aid thee tear for tear" };

val richardIII =
  { name    = "RichardIII",
    born    = 1452,
    crowned = 1483,
    died    = 1485,
    quote   = "Plots have I laid..." };

val {name=nameV, born=bornV, ...} = henryV;

val {name,born,died,quote,crowned} = richardIII;

#quote richardIII;

(* declaring a record type *)

type king =
  { name    : string,
    born    : int,
    crowned : int,
    died    : int,
    quote   : string };

fun lifetime(k: king) = #died k - #born k;

lifetime henryV;
lifetime richardIII;

(* 2.10 Infix Operators *)

infix xor;
fun (p xor q) = (p orelse q) andalso not (p andalso q);

true xor false xor true;

(*operator precedence*)
infix 6 plus;

fun (a plus b) = "(" ^ a ^ "+" ^ b ^ ")";

"1" plus "2" plus "3";

infix 7 times;
fun (a times b) = "(" ^ a ^ "*" ^ b ^ ")";

"m" times "n" times "3" plus "i" plus "j" times "k";

infixr 8 pow;
fun (a pow b) = "(" ^ a ^ "#" ^ b ^ ")";

"m" times "i" pow "j" pow "2" times "n";

infix ++;
fun ((x1,y1) ++ (x2,y2)) : vec = (x1+x2, y1+y2);

b ++ (0.1,0.2) ++ (20.0,30.0);

1+ ~3;

(*op keyword changes infix operator to prefix*)
op++ ((2.5,0.0), (0.1,2.5));
op^ ("Mont","joy");

(*nonfix permanently change infix to prefix*)
(*
nonfix *;
*(3,2);
*)

(* THE EVALUATION OF EXPRESSIONS *)

fun sqr(x) : int = x*x;

fun zero(x: int) = 0;

(* 2.12 recursive functions under call-by-value *)

fun fact n =
  if n=0 then 1 else n * fact(n-1);

fact 7;
fact 12;

fun facti (n,p) =
  if n=0 then p else facti(n-1, n*p);

facti(4,1);

fun cond (p,x,y): int = if p then x else y;
fun badf n = cond(n=0, 1, n*badf(n-1));

fun even n = (n mod 2 = 0);

even 5;
even 4;

fun powoftwo n =
  (n=1) orelse
  (even(n) andalso powoftwo(n div 2));


(*)
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

(* 2.19 Simultaneous declarations *)

val pi = 4.0 * Math.atan 1.0
and e = Math.exp 1.0
and log2 = Math.ln 2.0;

val one = "BONG ";
val three = one^one^one;
val five = three^one^one;

val one = three and thee = one;

fun pos d = neg(d-2.0) + 1.0/d
and neg d = if d>0.0 then pos(d-2.0) - 1.0/d else 0.0;

4.0 * pos(201.0);
4.0 * neg(8003.0);

fun sum(d,one) = if d>0.0 then sum(d-2.0, ~one) + one/d else 0.0;

(* Exercise 2.23 *)

fun P n = 1 + sumup(n-1)
and sumup k = if k<1 then 0 else P(k) + sumup(k-1);

P 5;

(* INRODUCTION TO MODULES *)

type complex = real*real;
val complexzero = (0.0, 0.0);

(* 2.21 Structures *)

structure Complex =
  struct
  type t = real*real;
  val zero = (0.0, 0.0);
  fun sum ((x,y), (x',y')) = (x+x', y+y') : t;
  fun diff ((x,y), (x',y')) = (x-x', y-y') : t;
  fun prod ((x,y), (x',y')) = (x*x' - y*y', x*y' + x'*y) : t;
  fun recip (x,y) =
    let val t = x*x + y*y
    in (x/t, ~y/t) end
  fun quo (z,z') = prod(z, recip z');
  end;

val i = (0.0, 1.0);
val a = (0.3, 0.0);

val b = Complex.sum(a,i);
Complex.sum(b, (0.7, 0.0));
Complex.prod(it,it);

signature ARITH =
  sig
  type t
  val zero : t
  val sum : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo : t * t -> t
  end;

(* Exercise 2.24: Declare a structure Real, matching signature ARITH, such that
Real. t is the type realand the components zero, sum, prod, etc., denote the
corresponding operations on type real. *)

structure Real : ARITH =
  struct
  type t = real;
  val zero = 0.0;
  fun sum   (x,y) = x+y: t;
  fun diff  (x,y) = x-y: t;
  fun prod  (x,y) = x*y: t;
  fun quo   (x,y) = x/y: t;
  end;

(* Exercise 2.25 Complete the declaration of structure Rational above, basing
your definitions on the laws n/d + n′/d′ = (nd′ + n′d)/dd′, (n/d) × (n′/d′) = nn′/dd′,
and 1/(n/d) = d/n. Use the function gcdto maintain the fractions in lowest terms,
and ensure that the denominator is always positive.*)

structure Rational : ARITH =
  struct
  type t = int*int;
  val zero = (0, 1);
  fun gcd(m,n) =
    if m=0 then  n  else gcd(n mod m, m);
  fun canon (m,n) =
      let val d = gcd(abs m, n)
      in  (m div d, n div d)  end
  fun sum   ((m,n), (m',n')) = canon(m*n' + m'*n, n*n');
  fun diff  ((m,n), (m',n')) = canon(m*n' - m'*n, n*n');
  fun prod  ((m,n), (m',n')) = canon(m*m', n*n');
  fun recip (m,n): t = if m<0 then (~n,~m) else (n,m)
  fun quo   (x,x') = prod(x, recip x');
  end;

(* POLYMORPHIR TYPES *)

fun pairself x = (x,x);
pairself 4.0;
pairself 7;

val pp = pairself ("Help!",999);

fun fst (x,y) = x;
fun snd (x,y) = y;

fst pp;
fst(snd pp);

fun fstfst z = fst(fst z);
fstfst pp;

fun silly x = fstfst(pairself(pairself x));

silly "Hold off your hands.";

fun I x = x;















(**)
