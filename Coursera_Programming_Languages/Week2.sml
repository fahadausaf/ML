(* My first ML program *)

val x = 94;
val y = 17;
val z = (x+y) + (y+2);
val q = z+1;
val abs_of_z = if z<0 then 0-z else z;
val abs_of_z_simpler = abs z;

(* Shadowing *)

val a = 10
val b = a*2
val a = 5
val c = b
val d = a
val a = a+1
val f = a*2

(* Functions *)

val x = 7

fun pow(x: int, y: int) =
  if y=0
  then 1
  else x * pow(x,y-1)

fun cube(x: int) =
  pow(x,3)

val sixtyfour = cube 4

val fortytwo = pow(2,2+2) + pow(4,2) + cube(2) + 2

(* Pairs *)

fun swap (pr: int*bool) = (#2 pr, #1 pr);

fun sum_two_pairs (pr1 : int * int, pr2 : int * int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2);

fun div_mod (x: int, y: int) =
  (x div y, x mod y);

fun sort_pair(pr: int * int) =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr);

swap (7,true);
swap (~4, false);
sort_pair(3,4);
val x = (4,3);
sort_pair x;
sort_pair (4,3);

val x1 = (7, (true,9));
val x2 = #1 (#2 x1);
val x3 = (#2 x1);
val x4 = ((3,5),((4,8),(0,0)));

(* Lists *)

[];
[3,4,5];
[4,3];
[3,4,5,6];
[(1+2),3+4,7];
[true,false,true];
val x = [7,8,9];
5::x;
6::5::x;

[6]::[[7,5],[5,2]];

null x;
null [];

x;
hd x;
tl x;
hd(tl x);
tl(tl x);
tl(tl(tl x));
