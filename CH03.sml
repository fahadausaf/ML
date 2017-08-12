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



















(**)
