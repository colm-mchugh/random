use "hw2provided.sml";

fun all_except_option(word, words) =
  let fun build_words(words, accum) =
  case words of
      [] => NONE
   |  w::words' => if same_string (w, word)
		   then SOME (accum @ words')
		   else build_words(words', w::accum)
  in
      build_words(words, [])
  end
      

fun get_substitutions1(subs, word) =
  case subs of
      [] => []
    | words::subs' => let val syns = all_except_option(word, words) in
			  case syns of
			      NONE => get_substitutions1(subs', word)
			   |  SOME w => w @ get_substitutions1(subs', word)
		      end
			  
fun get_substitutions2(subs, word) =
  let fun build_subs(subs, acc) =
	case subs of
	    [] => acc
	 |  words::subs' => let val wl = all_except_option(word, words) in
				case wl of
				    NONE => build_subs(subs', acc)
				 |  SOME w => build_subs(subs', w @ acc)
			    end
  in
      build_subs(subs, [])
  end

fun similar_names(subs, {first=f, middle=m, last=l}) =
  let val syns = get_substitutions1(subs, f) in
      case syns of
	  [] => []
	| _ => let fun make_names(syns, acc) =
		     case syns of
			 [] => acc
		      |  s::syns' => make_names(syns', {first=s, middle=m, last=l} :: acc)
	       in
		   {first=f, middle=m, last=l} :: make_names(syns, [])
	       end
  end
      
				
