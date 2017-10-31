[3,4];
[4,3];
[3,4,3];
[3,3,4];
[];

[(1,"One"), (2,"Two"), (3,"Three")];
[[3.1], [], [5.7, ~0.6]];

[ Math.sin 0.5, Math.cos 0.5, Math.exp 0.5 ];

fun upto (m,n) = if m>n then [] else m :: upto(m+1,n);

upto(2,5);

(* 3.2 Operating on Lists *)

fun prodof3 [i,j,k] : int = i*j*k;

fun prod [] = 1
  | prod (n::ns) = n * (prod ns);

prod[2,3,5];

fun maxl [m]: int = m
  | maxl (m::n::ns) = if m>n then maxl(m::ns) else maxl(n::ns);

maxl [~4, 0, ~12];
(*)
fun factl (n) = prod(upto(1,n));
factl 7;

explode "Banquo";

(* SOME FUNDAMENTAL LIST FUNCTIONS *)

(* 3.3 Testing lists and taking them apart *)

fun null [] = true
  | null (_::_) = false;

fun hd (x::_) = x;

hd[[[1,2],[3]], [[4]]];
hd it;
hd it;

fun tl (_::xs) = xs;

tl ["Out", "damned", "spot!"];
tl it;
tl it;

fun prod ns = if null ns then 1 else (hd ns) * (prod (tl ns));

prod [1,2,3,4,5];

(* Exercise 3.1 Write a version of maxl using null, hd and tl,
  instead of pattern-matching. *)

fun maxl l : int =
    if null(tl l) then hd l
    else if hd l > hd(tl l) then maxl(hd l :: tl(tl l))
                            else maxl(tl l);

maxl [1,2,3,4,5];

(* Exercise 3.2 Write a function to return the last element of a list. *)

fun last [x]    = x
  | last(_::xs) = last xs;

(* 3.4 List processing by numbers *)

fun nlength [] =0
  | nlength (x::xs) = 1 + nlength xs;

nlength[[1,2,3], [4,5,6]];

local
  fun addlen (n, []) = n
    | addlen (n, x::l) = addlen (n+1, l)
in
  fun length l = addlen (0,l)
end;

fun take ([], i) = []
  | take (x::xs, i) = if i>0 then x::take(xs, i-1) else [];

take (explode "Throw physic to the dogs!", 5);
take([9,8,7,6],3);

fun rtake ([], _, taken) = taken
  | rtake (x::xs, i, taken) = if i>0 then rtake(xs, i-1, x::taken) else taken;

rtake([9,8,7,6],3,[]);

fun drop ([], _) = []
  | drop (x::xs, i) = if i>0 then drop (xs, i-1)  else x::xs;

take (["Never","shall","sun","that","morrow","see!"], 3);
drop (["Never","shall","sun","that","morrow","see!"], 3);

(* Exercise 3.4 Write a function nth (l, n) to return the nth element
   of l (where the head is element 0) *)

fun nth([], _) = []
  | nth(x::xs, n) = if(n=1) then [x] else nth(xs, n-1);

nth (["Never","shall","sun","that","morrow","see!"], 3);

(* 3.5: Append and Reverse *)

[2,4,6] @ [8,10];

fun nrev [] = []
  | nrev (x::xs) = (nrev xs) @ [x];

nrev [0,1,2,3,4,5,6,7,8,9];

fun revAppend ([], ys) = ys
  | revAppend (x::xs, ys) = revAppend (xs, x::ys);

revAppend (["Macbeth","and","Banquo"],["all","hail!"]);

fun rev xs = revAppend(xs, []);

infix @;
fun xs @ [] = xs
  | [] @ ys = ys
  | (x::xs) @ ys = x :: (xs@ys);

(* 3.6Lists of lists, lists of pairs *)

fun concat [] = []
  | concat (l::ls) = l @ concat ls;

concat [["When","shall"], ["we","three"], ["meet","again"]];

fun zip(x::xs,y::ys) = (x,y) :: zip(xs,ys)
  | zip _ = [];

zip ([1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]);

fun conspair ((x,y), (xs,ys)) = (x::xs, y::ys);

fun unzip [] = ([],[])
  | unzip (pair::pairs) = conspair(pair, unzip pairs);

fun unzip [] = ([],[])
  | unzip ((x,y)::pairs) =
      let val (xs,ys) = unzip pairs
      in (x::xs, y::ys) end;

fun rev_unzip ([],xs,ys) = (xs,ys)
  | rev_unzip ((x,y)::pairs, xs, ys) =
      rev_unzip(pairs, x::xs, y::ys);

(* Exercise 3.9 Give an equivalent definition of zip that does not depend upon
the order in which patterns are considered. *)

fun zip ([], _)      = []
  | zip (_, [])      = []
  | zip(x::xs,y::ys) = (x,y) :: zip(xs,ys);

(* APPLICATIONS OF LISTS *)

(* 3.7: Making Change *)

fun change (coinvals, 0) = []
  | change (c::coinvals, amount) =
      if amount<c then change (coinvals, amount)
      else c::change(c::coinvals, amount-c);

val gb_coins = [50,20,10,5,2,1]
and us_coins = [25,10,5,1];

change (gb_coins, 43);
change (us_coins, 43);

(*
change([5,2], 16);
*)

(* Backtracking *)

fun allChange (coins, coin , 0) = [coins]
  | allChange (coins, [], amount) = []
  | allChange (coins, c::coinvals, amount) =
      if amount<0 then []
      else allChange(c::coins, c::coinvals, amount-c) @
           allChange(coins, coinvals, amount);

allChange([], [10,2], 27);

allChange([], [5,2], 16);
allChange([], gb_coins, 16);

(*
Exercise 3.11 Write a function to express integers as Roman numerals. Supplied
with suitable arguments, your function should be able to express 1984 as either
MDCCCCLXXXIIII or MCMLXXXIV.
*)

fun roman (numpairs, 0) = ""
  | roman ((s,v)::numpairs, amount) =
      if amount<v then roman(numpairs, amount)
      else s ^ roman((s,v)::numpairs, amount-v);

val rompairs1 =
    [("M",1000), ("D",500), ("C",100), ("L",50), ("X",10), ("V",5), ("I",1)]
and rompairs2 =
    [("M",1000), ("CM",900), ("D",500), ("CD",400),
     ("C",100),  ("XC",90),  ("L",50),  ("XL",40),
     ("X",10),   ("IX",9),   ("V",5),   ("IV",4),
     ("I",1)];

roman (rompairs1, 0);






(**)
