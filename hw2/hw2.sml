(* Homework 2 *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1a *)
fun all_except_option(s, ss) =
    case ss of 
         [] => NONE 
       | x :: xs => 
           if same_string(x, s) then SOME xs 
           else 
              case all_except_option(s, xs) of 
                   NONE => NONE
                 | SOME ys => SOME (x :: ys)

(* 1b  *)
fun get_substitutions1 (sss, s) = 
    case sss of 
       [] => []
       | x :: xs =>
           case all_except_option(s, x) of 
                NONE => get_substitutions1(xs, s)
              | SOME y => y @ get_substitutions1(xs, s)

(* 1c *)
fun get_substitutions2 (sss, s) = 
    let fun helper(xss, acc) = 
      case xss of 
          [] => acc
        | x::xs => 
            case all_except_option(s, x) of 
                 NONE => helper(xs, acc)
               | SOME ys => helper(xs, acc @ ys)
    in 
      helper(sss, [])
    end

(* 1d *) 
fun similar_names(sss, {first:string, middle:string, last:string}) =
    let 
      fun helper(s) = 
        case s of 
             [] => []
           | x::xs => {first=x, middle=middle, last=last}::helper(xs)
    in 
      {first=first, middle=middle, last=last}::helper(get_substitutions2(sss, first))
    end 


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2a *)
fun card_color card = 
    case card of 
         (Clubs, _) => Black 
       | (Spades, _) => Black 
       | (_, _) => Red

(* 2b *)
fun card_value card = 
    case card of
         (_, Ace) => 11
       | (_, King) => 10
       | (_, Queen) => 10
       | (_, Jack) => 10
       | (_, Num n) => n 

(* 2c *)
fun remove_card (cs, c, e) = 
    case cs of 
         [] => raise e 
       | x::xs => if x = c then xs else x::remove_card(xs, c, e)

(* 2d *)
fun all_same_color cs = 
    case cs of 
         [] => true 
       | _::[] => true 
       | c::c2::cs' => card_color(c) = card_color(c2) andalso
       all_same_color(c2::cs')

(* 2e *)
fun sum_cards cs = 
   let fun helper (xs, acc) = 
      case xs of 
           [] => acc 
         | x::xs' => helper(xs', acc+card_value(x))
   in 
     helper(cs, 0)
   end 

(* 2f *)
fun score (cs, goal) = 
    let 
      val sum = sum_cards cs 
      val prelim_score = if sum > goal then 3 * (sum-goal) else goal-sum 
    in 
      if all_same_color cs then prelim_score div 2 else prelim_score
    end 

(* 2g *) 
fun officiate (cs, ms, goal) = 
    let 
      fun helper(cs, ms, held) =
        let 
          val points = score(held, goal) 
          val sum = sum_cards(held)
        in
          case (ms, cs) of 
                ([], _) => points 
              | ((Draw)::ms', []) => points 
              | ((Draw)::ms', c::cs') => 
                  if sum+card_value(c) > goal 
                  then score(c::held, goal) 
                  else helper(cs', ms', c::held)
              | ((Discard c)::ms', cs') => helper(cs', ms', remove_card(held, c, IllegalMove))
        end
    in 
      helper(cs, ms, [])
    end 

(* 3a *)
fun score_challenge (css, goal) = 
    let
      fun adjust_sum (cs, sum) = 
        case cs of 
             [] => sum
           | ((_, Ace)::cs') =>
                if (abs (sum-10-goal)) < (abs (sum-goal)) 
                then adjust_sum(cs', sum-10)
                else sum 
           | (c::cs') => adjust_sum(cs', sum)
      val better_sum = adjust_sum(css, sum_cards css) 
      val prelim_score = if better_sum > goal then 3*(better_sum-goal) else (goal-better_sum)
    in 
      if all_same_color css then prelim_score div 2 else prelim_score
    end

fun officiate_challenge (cs, ms, goal) = 
    let 
      fun helper(cs, ms, held) = 
        let 
          val points = score_challenge(held, goal)
          val sum = sum_cards(held) 
        in 
          case (ms, cs) of 
               ([], _) => points 
             | ((Draw)::ms', []) => points 
             | ((Draw)::ms', c::cs') => 
                 if sum+card_value(c) > goal 
                 then score_challenge(c::held, goal) 
                 else helper(cs', ms', c::held) 
             | ((Discard c)::ms', cs') => helper(cs', ms', remove_card(held, c, IllegalMove)) 
        end 
    in 
      helper(cs, ms, [])
    end

(* 3b *)
(* returns SOME c if replacing c with x in hand leads to a score of 0. 
*  NONE if no such c exists *)
fun zero_if_replaced(hand, x, goal) = 
    let 
      fun helper(cs, acc) = 
        case cs of 
             [] => NONE 
           | c::cs' => 
               if score(x::cs'@acc, goal) = 0
               then SOME c 
               else helper(cs', c::acc)
    in 
      helper(hand, []) 
    end 

fun careful_player (css, goal) = 
    let 
      fun next_move(ms, cs, goal, held) = 
        if score(held, goal) = 0 then ms 
        else 
          case cs of 
              [] => ms 
             |(suit, rank)::cs' => 
                case zero_if_replaced(held, (suit, rank), goal) of 
                     NONE => 
                        if sum_cards held + card_value((suit, rank)) > goal 
                        then ms 
                        else 
                          if goal - sum_cards held > 10 
                          then next_move(ms @ [Draw], cs', goal, held @ [(suit,rank)])
                          else
                            if card_value((suit, rank)) < 10 
                            then next_move(ms @ [Draw], cs', goal, held@[(suit,rank)])
                            else ms (*maybe replace with a discard heuristic*)
                   | SOME c => 
                       next_move(ms @ [Discard c], cs, goal, remove_card(held, c, IllegalMove))
    in 
      next_move([], css, goal, [])
    end
