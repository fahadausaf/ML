(*

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

(* List Functions *)

fun sum_list (xs: int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs);

sum_list [1,2,3,4,5];

fun list_product(xs: int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs);

list_product [1,2,3,4,5];

fun countdown (x: int) =
  if x=0
  then []
  else x :: countdown(x-1);

countdown 7;
sum_list (countdown 700);

fun append (xs: int list, ys: int list) =
  if null xs
  then ys
  else (hd xs) :: append((tl xs), ys);

append([1,2,3,4],[5,6,7,8]);

fun sum_pair_list (xs: (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs);

sum_pair_list [(3,4),(5,6)];

fun firsts (xs : (int * int) list) =
  if null xs
  then []
  else (#1 (hd xs))::(firsts(tl xs))

fun seconds (xs : (int * int) list) =
  if null xs
  then []
  else (#2 (hd xs))::(seconds(tl xs))

fun sum_pair_list2 (xs : (int * int) list) =
  (sum_list (firsts xs)) + (sum_list (seconds xs))


(* Let Expressions *)


fun silly1 (z : int) =
  let
    val x = if z > 0 then z else 34
    val y = x+z+9
  in
    if x > y then x*2 else y*y
  end

fun silly2 () =
  let
    val x = 1
  in
    (let val x = 2 in x+1 end) + (let val y = x+2 in y+1 end)
  end

silly2 ();

(* Nested Functions *)

fun count (from: int, to: int) =
  if from=to
  then to::[]
  else from::count(from+1, to);

fun countup_from1(x: int) =
  count(1,x);

countup_from1 7;

fun countup_from1b(x: int) =
  let fun count (from: int, to: int) =
    if from=to
    then to::[]
    else from::count(from+1, to)
  in
    count(1,x)
  end;

countup_from1b 7;

fun countup_from1_better (x : int) =
  let fun count (from:int) =
    if from=x
    then x::[]
    else from :: count(from+1)
  in
    count 1
  end;

countup_from1_better 7;
*)

(* Let and Effeciency *)
(* Section 1: Let Expressions to Avoid Repeated Computation *)

fun countup(from : int, to : int) =
  if from=to
  then to::[]
  else from :: countup(from+1,to)

fun countdown(from : int, to : int) =
  if from=to
  then to::[]
  else from :: countdown(from-1,to)

(* badly named: evaluates to 0 on empty list *)
fun bad_max (xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else if hd xs > bad_max(tl xs)
  then hd xs
  else bad_max(tl xs)

(* badly named: evaluates to 0 on empty list *)
fun good_max (xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else
(* for style, could also use a let-binding for (hd xs) *)
  let val tl_ans = good_max(tl xs)
  in
    if hd xs > tl_ans
    then hd xs
    else tl_ans
  end
