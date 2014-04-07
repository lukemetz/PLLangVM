
structure StackRepresentation = struct


  datatype value = VInt of int
		 | VList of value list 
		 | VCode of sentence
		 | VClosure of sentence * value Vector.vector

  and sentence = SEmpty
	       | SSequence of word * sentence
	       | SIf of sentence * sentence * sentence
	       | SQuote of string * sentence
               | SCall of sentence
	       | SDefine of string * sentence * sentence

  and word = WInt of int
           | WPrim of string * (value list -> value list)
           | WDefined of string

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VList vs) = 
        "["^(String.concatWith "," (map stringOfValue vs))^"]"
    | stringOfValue (VCode s) = 
        "<"^(stringOfSentence s)^">"
    | stringOfValue (VClosure (s,v)) = 
      "<|"^(stringOfSentence s)^" | "
        ^(String.concatWith " " (Vector.foldr (fn (x,r) => 
						  (stringOfValue x)::r) [] v))
        ^"|>"

  and stringOfWord (WInt i) = Int.toString i
    | stringOfWord (WPrim (n,_)) = "primitive("^n^")"
    | stringOfWord (WDefined w) = w

  and stringOfSentence (SEmpty) = ""
    | stringOfSentence (SSequence (w,s)) = String.concat [stringOfWord w,
							  " ",
							  stringOfSentence s]
    | stringOfSentence (SIf (s1,s2,s3)) = String.concat ["IF ",
							 stringOfSentence s1,
							 "ELSE ",
							 stringOfSentence s2,
							 "THEN ",
							 stringOfSentence s3]
    | stringOfSentence (SQuote (w,s)) = String.concat ["' ", 
                                                       w,
                                                       " ",
                                                       stringOfSentence s]
    | stringOfSentence (SCall (ws)) = String.concat ["@ ", stringOfSentence ws]
    | stringOfSentence (SDefine (w,wsdef,ws)) = String.concat [":: ", 
							       w,
							       " ",
							       stringOfSentence wsdef,
							       ";; ",
							       stringOfSentence ws]

  fun stringOfStack [] _ = ""
    | stringOfStack _ 0 = "..."
    | stringOfStack (v::vs) d = String.concat [stringOfValue v, " ", 
					       stringOfStack vs (d-1)]
		       
end
