(* NOTE: These tests assume an outdated scoring heuristic given by 
*            score = | sum - goal | 
*        The updated hueristic is 
*            score = sum > goal ? 3*(sum-goal) : goal-sum 
*        The solution passes the autograder, so I will not update this file. 
*        This may, however, lead to exceptions if run on the current hw2.sml
*        file. 
*)
use "hw2.sml";

(* all_except_option tests *)
val aeo1 = isSome (all_except_option("hello", ["hi", "test"])) = false
val aeo2 = isSome (all_except_option("hello", [])) = false
val aeo3 = valOf (all_except_option("hello", ["test", "hello", "test"])) =
  ["test", "test"]
val aeo4 = valOf (all_except_option("hello", ["hello", "noah", "schell"])) =
  ["noah", "schell"]
val aeo5 = valOf (all_except_option("hello", ["noah", "hello", "schell"])) =
  ["noah", "schell"]

(* get_substitutions1 tests *) 
val gs11 = get_substitutions1([["a", "b", "c"], ["b", "c"], ["a", "d"]], "a") = ["b",
"c", "d"]
val gs12 = get_substitutions1([], "") = []
val gs13 = get_substitutions1([["hello"], []], "") = []

(* get_substitutions2 tests*)
val gs21 = get_substitutions2([["a", "b", "c"], ["b", "c"], ["a", "d"]], "a") =
  ["b", "c", "d"]
val gs22 = get_substitutions2([], "") = []
val gs23 = get_substitutions2([["hello"], []], "") = []

(* similar_names tests*)
val sn1 = similar_names([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
["Freddie", "Fred", "F"]], {first="Fred", middle="W", last="Smith"}) =
[{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith",
middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F",
last="Smith", middle="W"}]
val sn2 = similar_names([], {first="Noah", middle="T", last="Schell"}) =
  [{first="Noah", last="Schell", middle="T"}]

(* card_color tests *)
val cc1 = card_color((Clubs, Jack)) = Black
val cc2 = card_color((Spades, Num 10)) = Black
val cc3 = card_color((Diamonds, Num 2)) = Red
val cc4 = card_color((Hearts, Ace)) = Red

(* card_value tests *)
val cv1 = card_value((Clubs, Jack)) = 10
val cv2 = card_value((Spades, Ace)) = 11 
val cv3 = card_value((Hearts, Num 2)) = 2 

(* remove_card tests *)
exception E
val rc1 = remove_card([(Spades, Jack), (Hearts, Num 1)], (Hearts, Num 1), E) =
  [(Spades, Jack)]
val rc2 = remove_card([], (Spades, Ace), E)=[(Hearts, Num 9)] handle E => true 
val rc3 = remove_card([(Spades, Ace)], (Hearts, Num 3), E)=[(Clubs, King)] handle E => true
val rc4 = remove_card([(Clubs, Ace), (Clubs, Ace)], (Clubs, Ace), E) = [(Clubs,
Ace)]

(* all_same_color tests*)
val asc1 = all_same_color([(Clubs, Ace), (Spades, Num 1), (Spades, Jack)]) = true 
val asc2 = all_same_color([(Hearts, Ace), (Hearts, Jack), (Diamonds, Num 1)]) =
  true 
val asc3 = all_same_color([]) = true
val asc4 = all_same_color([(Hearts, King), (Spades, Ace)]) = false 

(* sum_cards tests *)
val sumc1 = sum_cards([(Hearts, Num 2), (Spades, Ace), (Diamonds, Jack), (Clubs,
Num 3)]) = 26
val sumc2 = sum_cards([]) = 0

(* score tests *)
val s1 = score([(Hearts, Num 2), (Spades, Ace), (Diamonds, Jack), (Clubs, Num
3)], 10) = 16 
val s2 = score([(Hearts, Num 3), (Diamonds, Ace), (Diamonds, Jack), (Hearts, Num
3)], 10) = 8
val s3 = score([(Hearts, Num 2)], 10) = 4
val s4 = score([(Hearts, Num 2), (Clubs, Num 5)], 10) = 3

(* officiate tests *)
val o1 = officiate([], [Draw], 10) = 5
val o2 = officiate([(Hearts, Num 2)], [Draw, Discard (Hearts, Num 2)], 10) = 5 
val o3 = officiate([], [], 20) = 10
val o4 = officiate([(Hearts, Ace), (Clubs, Num 9)], [Draw, Draw, Discard
(Hearts, Ace)], 1) = 5
val o5 = officiate([(Hearts, Ace), (Clubs, Num 9)], [Draw, Draw, Discard
(Hearts, Ace)], 23) = 7 

(* score_challenge tests *)
val sc1 = score_challenge([(Hearts, Ace)], 2) = 0 
val sc2 = score_challenge([(Hearts, Ace)], 10) = 0
val sc3 = score_challenge([(Hearts, Ace), (Diamonds, Num 2)], 11) = 1 
val sc4 = score_challenge([(Clubs, Ace), (Diamonds, Num 2)], 11) = 2
)
(* officiate_challenge tests *)
(* not testing this function. I just replaced score with score_challenge *)

(* zero_if_replaced tests *)
val zir1 = (isSome (zero_if_replaced([], (Hearts, Ace), 10))) = false
val zir2 = valOf (zero_if_replaced([(Hearts, Num 2)], (Clubs, King), 10)) =
  (Hearts, Num 2)
val zir3 = valOf (zero_if_replaced([(Spades, Num 3), (Diamonds, Queen), (Clubs,
Num 2)], (Clubs, Num 3), 7)) = (Diamonds, Queen)

(* careful_player tests *)
(* not testing this function. too complex to test*) 
