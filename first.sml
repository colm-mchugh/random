(* Colm McHugh, Courser PL, HW1 *)

fun is_older(d1 : int*int*int, d2 : int*int*int) =
  if #1 d1 = #1 d2
  then if #2 d1 = #2 d2
       then #3 d1 < #3 d2
       else #2 d1 < #2 d2
  else #1 d1 < #1 d2

fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else (if #2 (hd dates) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months(dates: (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)
							   
fun dates_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(words : string list, n : int) =
  if n = 1
  then hd words
  else get_nth(tl words, n - 1)

fun date_to_string(date : int*int*int) =
  let val months = ["January", "February", "March", "April", "May", "June",
		    "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) =
  let val sum_next = sum - hd numbers
  in
      if sum_next <= 0
      then 0
      else 1 + number_before_reaching_sum(sum_next, tl numbers)
  end
	    
fun what_month(day : int) =
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, days_in_months) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int*int*int) list) =
  if null dates
  then NONE
  else let
      fun oldest_nonempty(dates : (int*int*int) list) =
	if null (tl dates)
	then hd dates
	else let val tl_ans = oldest_nonempty(tl dates)
	     in
		 if is_older(hd dates, tl_ans)
		 then hd dates
		 else tl_ans
	     end
  in
      SOME(oldest_nonempty dates)
  end
	   
fun contains(xs : int list, x : int) =
  if null xs
  then false
  else
      if x = hd xs
      then true
      else contains(tl xs, x)

fun filter_dups(xs : int list) =
  if null xs
  then []
  else if contains(tl xs, hd xs)
  then filter_dups(tl xs)
  else hd xs :: filter_dups(tl xs)

fun number_in_months_challenge(dates: (int*int*int) list, months : int list) =
  number_in_months(dates, filter_dups(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, filter_dups(months))

fun reasonable_date(date : (int*int*int)) =
  (* return the number of days in the given month. Use year to check for leap year *)
  let fun days_in_mon(year : int, month : int) =
	let val days_per_mon = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    (* return the m'th element of list l *)
	    fun get_nth(l : int list, m : int) =
	      if m = 1
	      then hd l
	      else get_nth(tl l, m - 1)			  
	in
	    (* check for leap year, otherwise get the days from the days_per_mon list *)
	    if (month = 2) andalso (year mod 1000 <> 0 andalso (year mod 400 = 0 orelse year mod 4 = 0))
	    then 29
	    else
		get_nth(days_per_mon, month)
	end
  in
      (#1 date) > 0 (* valid year *)
      andalso (#2 date > 0 andalso #2 date < 13) (* valid month *)
      andalso (#3 date > 0 andalso #3 date <= days_in_mon(#1 date, #2 date)) (* valid day of month *)
  end
      
