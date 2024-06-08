(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older((1,2,3), (1, 2, 3)) = false
val test1c = is_older((1, 2, 3), (1, 1, 3)) = false
val test1d = is_older((1, 2, 3), (1, 3, 3)) = true 
val test1e = is_older((1, 1, 3), (1, 1, 2)) = false 
val test1f = is_older((1, 1, 3), (1, 1, 4)) = true 

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month([], 2) = 0
val test2c = number_in_month([], 13) = 0
val test2d = number_in_month([(1, 1, 1), (1, 2, 1), (1, 3, 1), (1, 4, 1)], 13) =
  0
val test2e = number_in_month([(1, 1, 1), (1, 1, 2), (1, 1, 3), (1, 2, 1), (1, 4,
1)], 1) = 3

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months([], []) = 0
val test3c = number_in_months([], [2]) = 0
val test3d = number_in_months([(1, 1, 1)], []) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month([], 1) = []
val test4c = dates_in_month([(1, 1, 1)], 12) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5b = dates_in_months([], []) = []
val test5c = dates_in_months([], [1, 2, 3, 4]) = []
val test5d = dates_in_months([(1, 1, 1), (2, 2, 2), (3, 3, 3)], []) = []

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7b = date_to_string(2024, 2, 28) = "February 28, 2024"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum(12, [1, 2, 3, 4, 5, 6]) = 4

val test9 = what_month 70 = 3
val test9b = what_month 1 = 1 
val test9c = what_month 360 = 12 
val test9d = what_month 40 = 2

val test10 = month_range (31, 34) = [1,2,2,2]
val test10b = month_range (58, 65) = [2, 2, 3, 3, 3, 3, 3, 3]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11b = oldest([]) = NONE
val test11c = oldest([(2024, 2, 2), (2016, 12, 31), (2016, 11, 2), (2025, 3,
2)]) = SOME(2016, 11, 2)
val test11d = oldest([(2024, 2, 2), (2016, 12, 31), (2016, 12, 1), (2025, 3,
3)]) = SOME(2016, 12, 1)

val test12 = number_in_months_challenge
([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4, 2, 3, 4, 2, 2, 3, 3])
= 3

val test13 = reasonable_date(2024, 3, 2) = true 
val test13b = reasonable_date(~1, 1, 1) = false 
val test13c = reasonable_date(1, ~1, 1) = false
val test13d = reasonable_date(1, 1, ~1) = false 
val test13e = reasonable_date(2020, 13, 12) = false
val test13f = reasonable_date(2020, 2, 30) = false
val test13g = reasonable_date(400, 2, 29) = true 
val test13h = reasonable_date(2020, 2, 29) = true 
val test13i = reasonable_date(2100, 2, 29) = false 






























