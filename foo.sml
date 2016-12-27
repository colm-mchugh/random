(* put your solutions for problem 1 here *)

fun same_string(x : string, y : string)=
  x = y

fun all_except_option pair=
    let
	(*tail recursion helper function *)
	(* It returns either the empty list or the list without the string  *)
	fun all_except_option_tail (pair, accum)=
	    case pair of
		(x,[]) => []
	      | (x,y::z) => if same_string(x,y)
			    then
				accum @ z
			    else
				all_except_option_tail((x,z), accum @ [y])						      	    
    in
	case all_except_option_tail(pair,[]) of
	    [] => NONE
	  | x => SOME x
    end
	

fun get_substitutions1 arg =
    case arg of
	([],name) => []
      | (list1::rest_lists, name ) =>
	case all_except_option(name,list1) of
	    NONE => get_substitutions1 (rest_lists, name)
	 |  SOME x =>  x @ get_substitutions1 (rest_lists ,name)

fun get_substitutions2 arg =
    let
	fun get_subst ( arg ,accum) =
	    case arg of
		([],name) => accum
	      | (list1::rest_lists, name ) =>
		case all_except_option(name,list1) of
		    NONE => get_subst( (rest_lists,name) , accum)
		  | SOME x => get_subst( (rest_lists,name) , x @accum)	    	
    in
	get_subst (arg,[])
    end

	

type  fullname = {first: string,middle : string, last : string }	
	
fun similar_names ( names_list : string list list, {first = x, middle = y, last = z} )=
    let
	val name = {first = x, middle = y, last = z}

	val subsitutions = get_substitutions2(names_list,x)
					     
	fun aux ( subs_list, accum)=
	    case  ( subs_list, name) of
		([],fullname) => accum 
	      | ( xs::xs' , fullname ) =>
		aux(  xs',
		      accum @ [{first = xs, middle=y,last=z }]  )
		   

    in
	aux( subsitutions, [name] )
    end
	
		


	     

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (card_suit,card_rank)=
    case card_suit of
	Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black

		      
fun card_value(card_suit,card_rank) =
    case card_rank of
	Num x => x
      | Ace => 11
      | _ => 10

fun remove_card( cs : card list , c : card , e :exn ) =
    case cs of
	[] => raise e
      | (xs::xs') => if xs = c
		     then
			 xs'
		     else
			 xs::remove_card(xs',c,e)

fun all_same_color cards_list=
    case cards_list of
	[] => true
       |[xs] => true 
       | (xs::xs'::rest) =>  ( card_color(xs) = card_color(xs') )
			     andalso all_same_color(xs::rest)


fun sum_cards cards_list=
    let
	fun aux(cards_list,accum)=
	    case cards_list of
		[] => accum
	      | xs::xs' =>   aux( xs', accum+card_value(xs) )
    in
	aux(cards_list,0)
    end

	
fun score (held_cards,goal)=
    let
	val sum_of_cards = sum_cards(held_cards);
	val div_by_two = all_same_color(held_cards);
	val prelim_score= if sum_of_cards > goal
			  then
			      3*(sum_of_cards - goal)
			  else
			      ( goal - sum_of_cards )
    in
	if div_by_two then prelim_score div 2 else prelim_score
    end

fun officiate (cards_list,move_list,goal )=
    let
	fun aux(cards_list : card list ,move_list : move list ,held_cards)=
	    case (cards_list,move_list) of
		( _ ,[] ) => score(held_cards,goal)
	      | ([],Draw::xs') => score(held_cards,goal)
				  
	      | (c::c',Draw::xs') => let
		  val held_cards = c::held_cards
		  val current_score = score(held_cards,goal)
	      in
		  if current_score > goal
		  then
		      current_score
		  else
		      aux( c',xs',held_cards)
	      end
				    
	      | (_,(Discard c)::xs' ) => aux( cards_list, xs',
					      remove_card(held_cards,c,IllegalMove))


    in
	aux(cards_list,move_list,[])
    end
