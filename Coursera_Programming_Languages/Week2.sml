(* My first ML program *)

val x = 94;
val y = 17;
val z = (x+y) + (y+2);
val q = z+1;
val abs_of_z = if z<0 then 0-z else z;
val abs_of_z_simpler = abs z;

(* Shadowing *)

val a = 10;
val b = a*2;
val a = 5
val c = b
val d = a
val a = a+1;
val f = a*2;
