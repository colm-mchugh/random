(* Colm McHugh, Coursera PL, HW3 *)

(* 1 *)
fun only_capitals words =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) words

(* 2 *)
	      
fun longest_string1 words =
  foldl (fn(acc, s) => if String.size(acc) > String.size(s) then acc else s) "" words
	
(* 3 *)
       
fun longest_string2 words =
  foldl (fn(acc, s) => if String.size(acc) >= String.size(s) then acc else s) "" words

(* 4 *)
	
fun longest_string_helper f words =
  foldl (fn(acc, s) => if f(String.size(acc), String.size(s)) then acc else s) "" words
val longest_string3 = longest_string_helper(fn(x, y) => x > y)
val longest_string4 = longest_string_helper(fn(x, y) => x >= y)

(* 5 *)
					   
fun longest_capitalized words =
  let val f = longest_string1 o only_capitals
  in
      f words
  end

(* 6 *)
      
fun rev_string s =
  (implode o rev o explode) s

exception NoAnswer

(* 7 *)
	      
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => (case f(x) of
		     NONE => first_answer f xs'
		   | SOME v => v)

(* 8 *)
		    
fun all_answers f xs =
  let fun build_lists(acc, xs) =
	case xs of
	    [] => SOME acc
	  | x::xs' => case f(x) of
			   NONE => NONE
			 | SOME l => build_lists(acc @ l, xs') 
  in
      build_lists([], xs)
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

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
	
(* 9 *)
	
fun count_wildcards p =
      g (fn() => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
      g (fn() => 1) (fn x => String.size(x)) p

fun count_some_var (s, p) =
      g (fn() => 0) (fn x => if s = x then 1 else 0) p

(* 10 *)
	
fun check_pat p =
  let fun varlist p =
	case p of
	    Wildcard          => []
	 |  Variable x        => [x]
	 |  TupleP ps         => List.foldl (fn(p, vs) => varlist(p) @ vs) [] ps
	 |  ConstructorP(_,p) => varlist p
	 |  _ => []
		     
      fun no_repeats xs =
	case xs of
	    [] => true
	  | x::xs' => not (List.exists (fn y => x = y) xs') andalso no_repeats(xs')
  in
      (no_repeats o varlist) p
  end

(* 11 *)
      
fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (_, Variable s) => SOME [(s,v)]
    | (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if length(vs) = length(ps)
			       then
				   all_answers (fn x => let val (v:valu, p:pattern) = x in match(v, p) end) (ListPair.zip(vs, ps))
			       else NONE
    | (Constructor(sv, vv), ConstructorP(sp, vp)) => if sv = sp then match(vv, vp) else NONE
    | _  => NONE					   

(* 12 *)
		
fun first_match v ps =
  SOME (first_answer (fn p => match(v, p)) ps)
  handle NoAnswer => NONE
			 
