use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val oc1 = only_capitals ["Hello", "hi", "Noah", "noah", "Noah"] = ["Hello", "Noah", "Noah"]
val oc2 = only_capitals ["a", "b", "c"] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val ls11 = longest_string1 ["Noah", "Test", "Hi", "Super!", "abcdef", "ah"] = "Super!"
val ls12 = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val ls21 = longest_string2 ["Noah", "Test", "Hi", "Super!", "abcdef", "ah"] = "abcdef"
val ls22 = longest_string2 [] = ""

val test4a = longest_string3 ["A","bc","C"] = "bc"
val ls31 = longest_string3 ["Noah", "Test", "Hi", "Super!", "abcdef", "ah"] = "Super!"
val ls32 = longest_string3 [] = ""

val test4b = longest_string4 ["A","B","C"] = "C"
val ls41 = longest_string4 ["Noah", "Test", "Hi", "Super!", "abcdef", "ah"] = "abcdef"
val ls42 = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"
val lc1 = longest_capitalized ["ab", "abcdef", "hello", "nonsense"] = ""
val lc2 = longest_capitalized ["Hello", "thisisatest", "Whats"] = "Hello"

val test6 = rev_string "abc" = "cba"
val rs1 = rev_string "HELlo" = "olLEH"
val rs2 = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val fa1 = first_answer (fn x => SOME x) [] = NONE handle NoAnswer => true  
val fa2 = first_answer (fn x => NONE) [1, 2, 3, 4, 5, 6, 7] = NONE handle NoAnswer => true
val fa3 = first_answer (fn x => if x = "hello" then SOME x else NONE) ["hi",
"hey", "hello", "hi"] = "hello"

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val aa1 = all_answers (fn x => SOME [x]) [2, 3, 4] = SOME [2, 3, 4]
val aa2 = all_answers (fn x => NONE) [] = SOME [] 
val aa3 = all_answers (fn x => SOME [x]) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val cw1 = count_wildcards (TupleP [ConstP 5, Wildcard,TupleP [Wildcard, ConstP
3, UnitP], Wildcard, ConstP 2]) = 3
val cw2 = count_wildcards UnitP = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val cwavl1 = count_wild_and_variable_lengths (TupleP [ConstP 5, Wildcard,
Variable("abcdef"), Variable("abc!"), TupleP[Wildcard, Variable("a")]]) = 13
val cwavl2 = count_wild_and_variable_lengths UnitP = 0

val test9c = count_some_var ("x", Variable("x")) = 1
val csv1 = count_some_var ("h", Variable("x")) = 0 
val csv2 = count_some_var ("", UnitP) = 0
val csv3 = count_some_var ("a", TupleP[TupleP[Variable("a"), UnitP], UnitP,
ConstP 3, Wildcard, Wildcard]) = 1

val test10 = check_pat (Variable("x")) = true
val cp1 = check_pat (UnitP) = true
val cp2 = check_pat (TupleP[Variable("x"), Variable("x"), UnitP, ConstP 3]) =
  false

val test11 = match (Const(1), UnitP) = NONE
val m1 = match (Const 5, Wildcard) = SOME []  
val m2 = match (Unit, Wildcard) = SOME []
val m3 = match (Constructor ("Hello", Const 5), Wildcard) = SOME []
val m4 = match (Tuple [Const 3, Unit, Constructor("noah", Const 4)], Wildcard) = SOME []
val m5 = match (Const 5, Variable "y") = SOME [("y", Const 5)]
val m6 = match (Unit, Variable "x") = SOME [("x", Unit)]
val m7 = match (Constructor ("Hey", Unit), Variable "n") = SOME [("n", Constructor ("Hey", Unit))]
val m8 = match (Tuple [Const 3, Unit, Const 5], Variable "z") = SOME [("z", Tuple [Const 3, Unit, Const 5])]
val m9 = match (Unit, UnitP) = SOME []
val m10 = match (Const 5, UnitP) = NONE
val m11 = match (Constructor("test", Const 5), UnitP) = NONE
val m12 = match (Const 13, ConstP 13) = SOME []
val m13 = match (Const 1, ConstP 3) = NONE
val m14 = match (Tuple [Const 3, Const 4], TupleP [Wildcard, Wildcard]) = SOME []
val m15 = match (Tuple [Const 4, Constructor("hey", Const 3), Const 3, Const 5], TupleP[ConstP 4, ConstructorP("hey", ConstP 3), Wildcard, Variable "x"]) = SOME [("x", Const 5)]
val m16 = match (Tuple [Const 5], TupleP [ConstP 5, Wildcard]) = NONE 
val m17 = match (Constructor("test", Const 3), ConstructorP("test2", ConstP 3)) = NONE
val m18 = match (Constructor("test", Const 3), ConstructorP("test", ConstP 3)) = SOME []

val test12 = first_match Unit [UnitP] = SOME []
val fm1 = first_match (Const 5) [UnitP, Variable "x", Variable "Y"] = SOME[("x", Const 5)]
val fm2 = first_match (Const 3) [ConstructorP("test", ConstP 5), UnitP, Wildcard] = SOME []
val fm3 = first_match (Const 3) [ConstructorP("test", ConstP 5), UnitP] = NONE
