(* 1 *)
fun is_older(x : int*int*int, y : int*int*int) =
    if #1 x = #1 y
    then if #2 x = #2 y
	       then #3 x < #3 y
         else #2 x < #2 y
    else #1 x < #1 y

(* 2 *)
fun get(n : int, ns : int list) = 
  if n = 1
  then hd ns 
  else get(n-1, tl ns)

val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates 
  then 0 
  else 
    let 
      val day = #3 (hd dates)
      val x = if #2 (hd dates) = month 
              andalso day >= 1 andalso day <= get(month, days_per_month)
              then 1 
              else 0
    in 
      x + number_in_month(tl dates, month)
    end 

(* 3 *)
fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null months 
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates 
    then []
    else if #2 (hd dates) = month andalso #3 (hd dates) >= 1 andalso #3 (hd
    dates) <= get(month, days_per_month) 
    then hd dates :: dates_in_month(tl dates, month) 
    else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 *)
fun get_nth(strings : string list, n : int) = 
  if n = 1 
  then hd strings 
  else get_nth(tl strings, n-1)

(* 7 *)
fun date_to_string(date : int*int*int) = 
  let 
    val months = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"]
  in 
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)
  end 

(* 8 *)
fun number_before_reaching_sum(sum : int, nums : int list)=
  if hd nums >= sum
  then 0
  else 1 + number_before_reaching_sum(sum-hd nums, tl nums)

(* 9 *)
fun what_month(day : int) =
  number_before_reaching_sum(day, days_per_month) + 1

(* 10 *)
fun month_range(day1 : int, day2 : int) = 
  if day1 > day2
  then [] 
  else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest(dates : (int*int*int) list) = 
  if null dates 
  then NONE
  else 
    let val tl_ans = oldest(tl dates)
    in if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
    then tl_ans 
    else SOME (hd dates) 
  end

(* 12 *)
fun contains(x : int, xs : int list) = 
  if null xs 
  then false 
  else x = hd xs orelse contains(x, tl xs)

fun remove_duplicates(nums : int list) = 
  if null nums 
  then []
  else if contains(hd nums, tl nums)
  then remove_duplicates(tl nums) 
  else hd nums :: remove_duplicates(tl nums)

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) = 
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) = 
  dates_in_months(dates, remove_duplicates(months))

(* 13 *)
fun reasonable_date(date : (int*int*int)) = 
  let 
    val year = #1 date 
    val month = #2 date 
    val day = #3 date
    val leap_case = 
      (year mod 400 = 0 orelse (year mod 4 = 0 andalso not (year mod 100 = 0)))
      andalso month = 2 andalso day = 29
  in 
    year > 0 andalso day > 0
    andalso month <= 12 andalso month >= 1 
    andalso if not (number_in_month([date], month) = 1)
    then leap_case
    else true
  end 
