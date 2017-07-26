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
























(**)
