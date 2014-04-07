
structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)      

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

  structure I = InternalRepresentation

                
  (* match a compiled regular expression against a list of characters *)
                
  fun matchRE' re cs = let
    val prefix = R.prefix re List.getItem
    fun getMatch NONE = NONE
      | getMatch (SOME (mt, cs')) = let
          val {pos,len} = MatchTree.root mt
        in
          SOME (implode (List.take (pos,len)), cs')
        end
  in
    getMatch (prefix cs)
  end
                       
  (* match a string regular expression against a list of characters *)
                       
  fun matchRE re cs = matchRE' (R.compileString re) cs



  exception Parsing of string

  fun parseError msg = raise Parsing msg
                         
                         


  (* 
   *   A simple lexer
   *
   *)

  datatype token = T_LET 
                 | T_IN
                 | T_SYM of string 
                 | T_INT of int 
                 | T_TRUE 
                 | T_FALSE
                 | T_EQUAL
                 | T_IF 
                 | T_THEN
                 | T_ELSE
                 | T_LPAREN 
                 | T_RPAREN
                 | T_PLUS
                 | T_MINUS
                 | T_TIMES
		 | T_BACKSLASH
 		 | T_RARROW
                 | T_COMMA
		 | T_DEF
		 | T_DCOLON


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken T_IN = "T_IN"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_MINUS = "T_MINUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_BACKSLASH = "T_BACKSLASH"
    | stringOfToken T_RARROW = "T_RARROW"
    | stringOfToken T_COMMA = "T_COMMA"
    | stringOfToken T_DEF = "T_DEF"
    | stringOfToken T_DCOLON = "T_DCOLON"


                   
  fun whitespace _ = NONE
                     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "in" = SOME (T_IN)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "def" = SOME (T_DEF)
    | produceSymbol text = SOME (T_SYM text)
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun producePlus _ = SOME (T_PLUS)
  fun produceMinus _ = SOME (T_MINUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceComma _ = SOME (T_COMMA)
  fun produceDColon _ = SOME (T_DCOLON)

  fun produceBackslash _ = SOME (T_BACKSLASH)
  fun produceRArrow _ = SOME (T_RARROW)
                       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
                 ("=",                    produceEqual),
                 ("\\+",                  producePlus),
		 ("->",                   produceRArrow),
                 ("-",                    produceMinus),
                 ("::",                   produceDColon),
		 ("\\*",                  produceTimes),
		 ("\\\\",                 produceBackslash),
		 (",",                    produceComma),
                 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
                 ("~?[0-9]+",             produceInt),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen)]
  end
               
               
  fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs
                              of NONE => loop xs
                               | SOME (m,cs') => (f m,cs'))
  in
    loop tokens
  end
                    
                    
  fun lex []  = []
    | lex cs = let
        val (token,cs') = getToken cs
      in
        case token 
         of NONE => lex cs'
          | SOME t => t::(lex cs')
      end
               
               
  fun lexString str = lex (explode str)
                      
                      
                           



  (* 
   *   A SIMPLE PARSER FOR AN ML-LIKE SYNTAX (MULTI-ARGUMENT FUNCTIONS)
   * 
   *   Grammar:
   *
   *   decl ::= T_DEF T_SYM sym_list T_EQUAL expr 
   *            expr
   * 
   *   expr ::= eterm T_EQUAL term 
   *            eterm              
   *
   *   eterm ::= cterm T_DCOLON cterm
   *             cterm
   *
   *   cterm ::= term T_PLUS term  
   *             term T_MINUS term 
   *             term              
   *   
   *   term ::= factor T_TIMES factor
   *            factor
   *
   *   factor :: = aterm aterm_list  
   *            
   *   aterm ::= T_INT                              
   *             T_TRUE                             
   *             T_FALSE                            
   *             T_SYM                              
   *             T_BACKSLASH T_SYM T_RARROW expr    
   *             T_LPAREN expr T_RPAREN             
   *             T_IF expr T_THEN expr T_ELSE expr  
   *             T_LET T_SYM T_EQUAL expr T_IN expr 
   *             T_LET T_SYM sym_list T_EQUAL expr T_IN expr 
   * 
   *   aterm_list ::= aterm aterm_list            
   *                  <empty>                     
   *
   *   sym_list ::= T_SYM sym_list
   *                T_SYM   
   *
   *)


  datatype decl = DDef of string * string list * I.expr
                | DExpr of I.expr

  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_TRUE (T_TRUE::ts) = SOME ts
    | expect_TRUE _ = NONE

  fun expect_FALSE (T_FALSE::ts) = SOME ts
    | expect_FALSE _ = NONE

  fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
    | expect_SYM _ = NONE

  fun expect_IF (T_IF::ts) = SOME ts
    | expect_IF _ = NONE

  fun expect_THEN (T_THEN::ts) = SOME ts
    | expect_THEN _ = NONE

  fun expect_ELSE (T_ELSE::ts) = SOME ts
    | expect_ELSE _ = NONE

  fun expect_LET (T_LET::ts) = SOME ts
    | expect_LET _ = NONE

  fun expect_EQUAL (T_EQUAL::ts) = SOME ts
    | expect_EQUAL _ = NONE

  fun expect_IN (T_IN::ts) = SOME ts
    | expect_IN _ = NONE

  fun expect_LPAREN (T_LPAREN::ts) = SOME ts
    | expect_LPAREN _ = NONE

  fun expect_RPAREN (T_RPAREN::ts) = SOME ts
    | expect_RPAREN _ = NONE

  fun expect_PLUS (T_PLUS::ts) = SOME ts
    | expect_PLUS _ = NONE

  fun expect_MINUS (T_MINUS::ts) = SOME ts
    | expect_MINUS _ = NONE

  fun expect_TIMES (T_TIMES::ts) = SOME ts
    | expect_TIMES _ = NONE

  fun expect_COMMA (T_COMMA::ts) = SOME ts
    | expect_COMMA _ = NONE

  fun expect_BACKSLASH (T_BACKSLASH::ts) = SOME ts
    | expect_BACKSLASH _ = NONE

  fun expect_RARROW (T_RARROW::ts) = SOME ts
    | expect_RARROW _ = NONE

  fun expect_DCOLON (T_DCOLON::ts) = SOME ts
    | expect_DCOLON _ = NONE

  fun expect_DEF (T_DEF::ts) = SOME ts
    | expect_DEF _ = NONE


  fun choose [] ts = NONE
    | choose (parser::parsers) ts = 
      (case parser ts
	of NONE => choose parsers ts
	 | s => s)


  fun parse_expr ts = let
    fun expr_equal ts = 
	(case parse_eterm ts
	  of NONE => NONE
	   | SOME (e1,ts) => 
             (case expect_EQUAL ts
               of NONE => NONE
		| SOME ts => 
		  (case parse_eterm ts
                    of NONE => NONE
                     | SOME (e2,ts) => SOME (I.EApp (I.EApp (I.EIdent "=", e1), e2),ts))))
    and expr_eterm ts = parse_eterm ts
  in
      choose [expr_equal, expr_eterm] ts
  end


  and parse_eterm ts = 
      (case parse_cterm ts
	of NONE => NONE
	 | SOME (e1,ts) => 
           (case expect_PLUS ts
             of NONE => 
		(case expect_MINUS ts
		  of NONE => SOME (e1,ts)
		   | SOME ts => 
		     (case parse_cterm ts
		       of NONE => NONE
			| SOME (e2,ts) => 
			  SOME (I.EApp (I.EApp (I.EIdent "-", e1), e2),ts)))
	      | SOME ts => 
		(case parse_cterm ts
                  of NONE => NONE
                   | SOME (e2,ts) => SOME (I.EApp (I.EApp (I.EIdent "+", e1), e2),ts))))

  and parse_cterm ts = 
      (case parse_term ts
	of NONE => NONE
	 | SOME (e1,ts) => 
	   (case expect_DCOLON ts
	     of NONE => SOME (e1,ts)
	      | SOME ts => 
		(case parse_term ts
		  of NONE => NONE
		   | SOME (e2,ts) => SOME (I.EApp (I.EApp (I.EIdent "::", e1), e2),ts))))

  and parse_term ts = 
      (case parse_factor ts
	of NONE => NONE
	 | SOME (e1,ts) => 
	   (case expect_TIMES ts
	     of NONE => SOME (e1,ts)
	      | SOME ts => 
		(case parse_factor ts
		  of NONE => NONE
		   | SOME (e2,ts) => SOME (I.EApp (I.EApp (I.EIdent "*", e1), e2),ts))))
  
  and parse_factor ts = let
    fun convert [] = parseError "empty list of aterms"
      | convert [at] = at
      | convert (at1::at2::ats) = convert ((I.EApp (at1,at2))::ats)
  in
    (case parse_aterm ts
      of NONE => NONE
       | SOME (at,ts) => 
         (case parse_aterm_list ts
           of NONE => NONE
            | SOME (ats,ts) => SOME (convert (at::ats),ts)))
  end
   
		      
  and parse_aterm ts = let
    fun parse_aterm_INT ts = 
	(case expect_INT ts 
	  of NONE => NONE
	   | SOME (i,ts) => SOME (I.EVal (I.VInt i),ts))
	
    and parse_aterm_TRUE ts = 
	(case expect_TRUE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool true),ts))
	
    and parse_aterm_FALSE ts = 
	(case expect_FALSE ts
	  of NONE => NONE
	   | SOME ts => SOME (I.EVal (I.VBool false),ts))
	
    and parse_aterm_SYM ts = 
	(case expect_SYM ts
	  of NONE => NONE
	   | SOME (s,ts) => SOME (I.EIdent s,ts))
	
    and parse_aterm_FUN ts = 
	(case expect_BACKSLASH ts 
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (s,ts) => 
		  (case expect_RARROW ts
		    of NONE => NONE
		     | SOME ts => 
		       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e,ts) => SOME (I.EFun (s,e), ts)))))
	
    and parse_aterm_PARENS ts = 
	(case expect_LPAREN ts
	  of NONE => NONE
	   | SOME ts =>
             (case parse_expr ts
               of NONE => NONE
		| SOME (e,ts) => 
		  (case expect_RPAREN ts
                    of NONE => NONE
                     | SOME ts => SOME (e,ts))))
	
    and parse_aterm_IF ts = 
	(case expect_IF ts
	  of NONE => NONE
	   | SOME ts => 
             (case parse_expr ts
               of NONE => NONE
		| SOME (e1,ts) => 
		  (case expect_THEN ts
                    of NONE => NONE
                     | SOME ts => 
                       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e2,ts) => 
                            (case expect_ELSE ts
                              of NONE => NONE
                               | SOME ts => 
				 (case parse_expr ts
				   of NONE => NONE
                                    | SOME (e3,ts) => SOME (I.EIf (e1,e2,e3),ts)))))))
	
    and parse_aterm_LET ts = 
	(case expect_LET ts 
	  of NONE => NONE
	   | SOME ts => 
             (case expect_SYM ts 
               of NONE => NONE
		| SOME (s,ts) => 
		  (case expect_EQUAL ts
                    of NONE => NONE
                     | SOME ts => 
                       (case parse_expr ts
			 of NONE => NONE
			  | SOME (e1,ts) => 
                            (case expect_IN ts
                              of NONE => NONE
                               | SOME ts => 
				 (case parse_expr ts
				   of NONE => NONE
                                    | SOME (e2,ts) => SOME (I.ELet (s,e1,e2),ts)))))))
	
    and parse_aterm_LET_FUN ts = 
	(case expect_LET ts 
	  of NONE => NONE
	   | SOME ts => 
             (case expect_SYM ts 
               of NONE => NONE
		| SOME (s,ts) => 
		  (case parse_sym_list ts
                    of NONE => NONE
                     | SOME (params,ts) => 
                       (case expect_EQUAL ts
			 of NONE => NONE
			  | SOME ts => 
                            (case parse_expr ts
                              of NONE => NONE
                               | SOME (e1,ts) => 
				 (case expect_IN ts
				   of NONE => NONE
                                    | SOME ts => 
                                      (case parse_expr ts
					of NONE => NONE
					 | SOME (e2,ts) => 
                                           SOME (I.makeLet s params e1 e2,ts))))))))
  in
    choose [parse_aterm_INT,
	    parse_aterm_TRUE,
	    parse_aterm_FALSE,
	    parse_aterm_SYM,
	    parse_aterm_FUN,
	    parse_aterm_PARENS,
	    parse_aterm_IF,
	    parse_aterm_LET,
	    parse_aterm_LET_FUN] ts
  end
		       
  and parse_aterm_list ts = let
    fun aterm_list_many ts = 
	(case parse_aterm ts
	  of NONE => NONE
	   | SOME (at,ts) => 
             (case parse_aterm_list ts
               of NONE => NONE
		| SOME (ats,ts) => SOME (at::ats,ts)))
    and aterm_list_empty ts = SOME ([], ts)
  in
    choose [aterm_list_many,
	    aterm_list_empty] ts
  end


  and parse_sym_list ts =
      (case expect_SYM ts 
	of NONE => NONE
	 | SOME (s,ts) => 
	   (case parse_sym_list ts
	     of NONE => SOME ([s],ts)
	      | SOME (ss,ts) => SOME (s::ss,ts)))


  and parse_decl ts = let
    fun decl_def ts = 
	(case expect_DEF ts
	  of NONE => NONE
	   | SOME ts => 
	     (case expect_SYM ts
	       of NONE => NONE
		| SOME (name,ts) => 
		  (case parse_sym_list ts
		    of NONE => 
		       (case expect_EQUAL ts
			 of NONE => NONE
			  | SOME ts => 
			    (case parse_expr ts
			      of NONE => NONE
			       | SOME (e,ts) => SOME (DDef (name,[],e),ts)))
		     | SOME (params,ts) => 
		       (case expect_EQUAL ts
			 of NONE => NONE
			  | SOME ts => 
			    (case parse_expr ts
			      of NONE => NONE
			       | SOME (e,ts) => SOME (DDef (name,params,e),ts))))))
    fun decl_expr ts = 
	(case parse_expr ts
	  of NONE => NONE
	   | SOME (e,ts) => SOME (DExpr e, ts))
  in
    choose [decl_def, decl_expr] ts
  end


  fun parseDecl str = 
      (case parse_decl (lexString str)
        of SOME (d,[]) => d
         | SOME (_,_)  => parseError "leftover characters past parsed expression"
         | NONE => parseError "cannot parse input")
      

end
