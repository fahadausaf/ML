(* Assignment 1 *)

fun is_older(d1: int*int*int, d2: int*int*int) =
  if (#1 d1) > (#1 d2) then true
  else if (#1 d1) = (#1 d2) andalso (#2 d1) > (#2 d2) then true
  else if (#1 d1) = (#1 d2) andalso (#2 d1) = (#2 d2) andalso (#3 d1) > (#3 d2) then true
  else false;

is_older((2016,11,21),(2016,11,13));
is_older((2016,11,21),(2016,11,21));

fun number_in_month (ds: (int*int*int) list, m: int) =
  if null ds then 0
  else
    if (#2 (hd ds)) = m
    then 1 + number_in_month(tl ds, m)
    else number_in_month(tl ds, m);

val dates = [(2016,11,21),(2016,11,13),(2016,12,21),(2016,12,13),(2016,11,21),(2016,9,13)];

number_in_month(dates, 11);
number_in_month(dates, 12);
number_in_month(dates, 9);

fun number_in_months (ds: (int*int*int) list, ms: int list) =
  if null ms then 0
  else number_in_month(ds, (hd ms)) + number_in_months((tl ds), (tl ms));

number_in_months(dates, [11,12]);
