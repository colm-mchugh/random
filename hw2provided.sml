(* Colm McHugh, Coursera PL, HW2 *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1 (a) *)
	     
fun all_except_option(word, words) =
  case words of
      [] => NONE
    | w::words' => if same_string(w, word)
		   then SOME words'
		   else case all_except_option(word, words') of
			    NONE => NONE
			 |  SOME ws => SOME(w::ws)

(* 1 (b) *)
					   
fun get_substitutions1(subs, word) =
  case subs of
      [] => []
    | words::subs' =>  case all_except_option(word, words) of
			  NONE => get_substitutions1(subs', word)
			| SOME w => w @ get_substitutions1(subs', word)

(* 1 (c) *)
			  
fun get_substitutions2(subs, word) =
  let fun build_subs(subs, acc) =
	case subs of
	    [] => acc
	 |  words::subs' => case all_except_option(word, words) of
				NONE => build_subs(subs', acc)
			     |  SOME w => build_subs(subs', acc @ w)
  in
      build_subs(subs, [])
  end

(* 1 (d) *)
      
fun similar_names(subs, {first=f, middle=m, last=l}) =
  {first=f, middle=m, last=l}::
  (case get_substitutions2(subs, f) of
      [] => []
    | synonyms => let fun make_names(syns) =
			case syns of
			    [] => []
			 |  s::syns' => {first=s, middle=m, last=l}::make_names(syns')
		  in
		      make_names(synonyms)
		  end
  )

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2 (a) *)
	      
fun card_color c =
  case c of
      (Clubs, _) => Black
   |  (Spades, _) => Black
   |  _ => Red

(* 2 (b) *)
	       
fun card_value c =
  case c of
      (_, Num i) => i
   |  (_, Ace) => 11
   |  _ => 10

(* 2 (c) *)
	       
fun remove_card(cs, c, ex) =
  let fun build_cards(cs, acc) =
	case cs of
	    [] => raise ex
	 |  c'::cs' => if c = c' then acc @ cs' else build_cards(cs', c'::acc)
  in
      build_cards(cs, [])
  end

(* 2 (d) *)
      
fun all_same_color cs =
  case cs of
      [] => true
   |  _::[] => true
   |  f::(s::rest) => card_color(f) = card_color(s)
		      andalso case rest of
				  x::[] => card_color(s) = card_color(x)
			       |  _  => all_same_color(rest)

(* 2 (e) *)
									  
fun sum_cards cs =
  let fun build_sum(cs, acc) =
	case cs of
	    [] => acc
	 |  c:: cs' => build_sum(cs', acc + card_value(c))
  in
      build_sum(cs, 0)
  end

(* 2 (f) *)
      
fun score (cs, goal) =
  let val sum = sum_cards(cs)
      val prelim_score = if (sum > goal) then 3 * (sum - goal) else goal - sum
  in
      prelim_score div (if all_same_color(cs) then 2 else 1)
  end

(* 2 (f) *)
      
fun officiate(cards, moves, goal) =
  let fun make_move(held, cards, moves) =
	case moves of
	    [] => score (held, goal)
	 |  Discard c :: moves' => make_move(remove_card(held, c, IllegalMove), cards, moves')
	 |  Draw :: moves' => case cards of
				  [] => score(held, goal)
				| c'::cards' => let val hn = c'::held
						    val s = score(hn, goal)
						in
						    if s > goal
						    then s
						    else make_move(hn, cards', moves')
						end
  in
      make_move([], cards, moves)
  end
      
