(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a *)
fun all_except_option(s : string, ss : string list) =
    case ss of 
         [] => NONE 
       | x :: xs => 
           if same_string(x, s) 
           then SOME xs 
           else 
             let val lst = all_except_option(s, xs) 
             in 
               case lst of 
                    NONE => SOME [x]
                  | SOME ys => SOME (x :: ys)
              end

(* 1b *) 
fun get_substitutions1 (sss : string list list, s : string) = 
    case sss of 
       [] => []
       | x :: xs =>
           let 
             val v = all_except_option(s, x)
           in 
             case v of 
                NONE => get_substitutions1(xs, s)
              | SOME y => y @ get_substitutions1(xs, s) 
           end 

(* 1c *)
fun get_substitutions2 (sss : string list list, s : string) = 
    let fun helper(xss : string list list, acc : string list) = 
      case xss of 
          [] => acc
        | x::xs => helper(xs, acc @ all_exception_option(s, x))
    in 
      helper(sss, [])
    end

(* 1d 
fun similar_names(sss : string list list, {first:string, middle: string,
  last:string}) = 
    let 
      fun generate_list(, acc : )
      val firsts = get_substitutions(sss, first) 
    in
      *)
      


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
