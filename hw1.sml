(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)

fun is_older (d1: int*int*int, d2: int*int*int)= 
    if (#1 d1) < (#1 d2) then true
    else if (#1 d1) = (#1 d2) andalso (#2 d1) < (#2 d1) then true
    else if (#1 d1) = (#1 d2) andalso (#2 d1) = (#2 d1) andalso (#3 d1) < (#3 d1) then true
    else false;

fun number_in_month(d: (int*int*int) list, m: int) = 
    if null d then 0
    else 
        if (#2 (hd d)) = m then 1 + number_in_month(tl d, m)
        else number_in_month(tl d, m);

fun number_in_months(d: (int*int*int) list, m: int list) = 
    if null m then 0
    else number_in_month(d, hd m) + number_in_months(d, tl m);



fun dates_in_month (d: (int*int*int) list, m: int) = 
    if null d then []
    else 
        if (#2 (hd d)) = m then hd d :: dates_in_month(tl d, m)
        else dates_in_month(tl d, m);


fun dates_in_months (d: (int*int*int) list, m: int list) = 
    if null m then []
    else dates_in_month(d, hd m) @ dates_in_months(d, tl m);

fun get_nth (s: string list, n: int) = 
    if n = 1 then hd s
    else get_nth(tl s, n-1);
    

fun date_to_string((year, month, day): int*int*int) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July","August", "September", "October", "November", "December"]
  in
     get_nth(months, month) ^ " " ^ (Int.toString (day)) ^ ", " ^ (Int.toString (year))
  end
  

fun number_before_reaching_sum (sum: int, l: int list) = 
     if null l then 0
     else
        let
            val tmp = number_before_reaching_sum((sum - hd l), (tl l))
        in 
            if sum - hd l > 0
            then tmp + 1
            else tmp 
        end




fun what_month(day: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end



fun month_range (d1: int, d2: int) = 
    if d1 > d2 then []
    else what_month(d1) :: month_range(d1 + 1, d2);



fun oldest(d: (int*int*int) list) =
  if null d then NONE
  else 
    let
      val tmp = oldest(tl d)
    in
      if isSome tmp andalso is_older(valOf tmp, hd d) then tmp
      else SOME (hd d)
    end
    


fun reasonable_date ((year, month, day): int*int*int) =
    let
        val dm  = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val dmp = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        fun get_nth(ints: int list, n: int) =
          if n = 1 then hd ints
          else get_nth(tl ints, n - 1)
        
        fun is_valid_date() = 
            if year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0
            then day <= get_nth(dmp, month)
            else day <= get_nth(dm, month)
    in
        year > 0 andalso month >= 1 andalso month <= 12 
        andalso day >= 1 andalso day <= 31 andalso is_valid_date()
    end









