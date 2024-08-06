(* problem 1 *)
fun only_capitals strings = 
List.filter (fn str => Char.isUpper (String.sub (str, 0))) strings 

(* problem 2 *)
fun longest_string1 strings = 
    List.foldl (fn (a, b) => if String.size a > String.size b then a else b) "" strings

(* problem 3 *)
fun longest_string2 strings = 
    List.foldl (fn (a, b) => if String.size b > String.size a then b else a) "" strings

(* problem 4 *)
fun longest_string_helper f strings = 
    List.foldl (fn (a, b) => if f(String.size a, String.size b) then a else b) "" strings 

val longest_string3 = longest_string_helper (fn (a, b) => a > b) 
val longest_string4 = longest_string_helper (fn (a, b) => a > b orelse a = b)

(* problem 5 *)
val longest_capitalized = longest_string1 o only_capitals 

(* problem 6 *)
fun rev_string str =
    ( String.implode o List.rev o String.explode ) str
    

exception NoAnswer

(* problem 7 *)
fun first_answer f xs =
    case xs of 
         [] => raise NoAnswer
       | x::xs' => case f x of 
                      SOME v => v
                    | NONE => first_answer f xs' 

(* problem 8 *)
fun all_answers f items = 
    let fun applyf (xs, acc) = 
        case xs of 
             [] => acc 
            | x::xs' => case f x of 
                            NONE => raise NoAnswer  
                          | SOME v => applyf(xs', acc @ v)
    in 
      SOME (applyf (items, [])) handle NotAnswer => NONE
    end
    
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p = (* f1 is applied to wildcards, f2 is applied to variables, goes
  through whole expression*)
    let 
	val r = g f1 f2 (* recursive function *) 
    in
	case p of 
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(* problem 9 *)
fun count_wildcards p =
    g (fn x => 1) (fn x => 0) p 

fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x => String.size x) p

fun count_some_var (str, p) = 
    g (fn x => 0) (fn x => if x=str then 1 else 0) p    
  
(* problem 10 *)
fun check_pat p = 
    let
      fun contains_repeats xs = 
          case xs of 
               [] => false 
             | x::xs' => (List.exists (fn y => y=x) xs') orelse contains_repeats xs'
      fun extract_vars pat = 
          case pat of 
               Wildcard => [] 
             | Variable x => [x]
             | UnitP => []
             | ConstP x => []
             | TupleP xs => List.foldl (fn (y, ys) => ys @ extract_vars y) [] xs
             | ConstructorP(_, x) => extract_vars x
    in
      not ((contains_repeats o extract_vars) p)
    end

(* problem 11 *)
fun match (valu, pat) = 
    case (valu, pat) of 
         (_, Wildcard) => SOME []
       | (v, Variable s) => SOME [(s, v)] 
       | (Unit, UnitP) => SOME []
       | (Const v, ConstP s) => 
           if v=s then SOME [] else NONE
       | (Tuple vs, TupleP ps) => 
           if (List.length vs) = (List.length ps) 
           then all_answers match (ListPair.zip(vs, ps))
           else NONE
       | (Constructor (s1, v), ConstructorP (s2, p)) => 
           if s1=s2 then match(v, p) else NONE
       | (_, _) => NONE

(* problem 12 *)
fun first_match v pats =
     let val pairs = List.map (fn x => (v, x)) pats
     in SOME (first_answer match pairs) handle NoAnswer => NONE
     end

(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* Working on the challenge problem, but I haven't finished it yet. 
(* loops through list of constructors and finds datatype for given constructor*)
fun get_datatype (cs, s) = 
    case cs of
         [] => Anything
       | (a, b, c)::cs' => if a=s then c else get_datatype(cs', s)

(* returns type of any pattern given a list of constructors *)
fun typecheck_pattern cs p = 
    case p of
         Wildcard => Anything
       | Variable _ => Anything 
       | UnitP => Anything (* unitT? *)
       | ConstP _ => Anything (* intT? *)
       | TupleP xs => TupleT (List.map (typecheck_pattern cs) xs)
       | ConstructorP (s, v) => 
           case get_datatype(cs, s) of
                TupleT xs => TupleT xs
                | _ => Anything

(* chooses the least inclusive type given two types *)
fun choose_type (t1, t2) = 
    case (t1, t2) of
         (Anything, x) => x
       | (x, Anything) => x
       | (TupleT x, TupleT y) => 
           if (List.length x) = (List.length y)
           then TupleT (List.map choose_type (ListPair.zip (x, y)))
           else raise NoAnswer
       | _ => Anything

fun typecheck_patterns (cs, ps) = 
    case List.map (typecheck_pattern cs) ps of 
         [] => NONE
       | x::[] => SOME x 
       | x::y::xs => 
           case typecheck_patterns (cs, (choose_type(x, y)::xs)) of
                NONE => NONE
              | SOME v => SOME v 
           handle NoAnswer => NONE

*)


