
(* A naive version of the compiler *)


structure Compiler = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation


  exception Compilation of string

  fun compileError msg = raise Compilation msg



  (* 
   *   helper functions for working with sentences
   * 
   *  appendS : sentence -> sentence -> sentence // appends two sentences
   *  appendSs : sentence list -> sentence       // appends list of sentences
   *  wordS : word -> sentence                   // sentence with only a word
   *)
  
  fun appendS (S.SEmpty) s = s
    | appendS (S.SSequence (w,s1)) s2 = S.SSequence (w,appendS s1 s2)
    | appendS (S.SIf (s1,s2,s3)) s4 = S.SIf (s1,s2,appendS s3 s4)
    | appendS (S.SQuote (w,s1)) s2 = S.SQuote (w,appendS s1 s2)
    | appendS (S.SCall s1) s2 = S.SCall (appendS s1 s2)
    | appendS (S.SDefine (w,sdef,s1)) s2 = S.SDefine (w,sdef,appendS s1 s2)

  fun appendSs [] = S.SEmpty
    | appendSs (s1::ss) = appendS s1 (appendSs ss)

  fun wordS w = S.SSequence (w,S.SEmpty)

  fun quoteS n = S.SQuote (n,S.SEmpty)

  fun definedS n = S.SSequence (S.WDefined n, S.SEmpty)
  fun intS i = S.SSequence (S.WInt i, S.SEmpty)


  (* shortcuts *)
  val $$ = appendSs



  (* compile a value into a sentence that produces that value *)

  fun compileV (I.VInt i) = intS i
    | compileV (I.VBool true) = intS 1
    | compileV (I.VBool false) = intS 0
    | compileV (I.VList []) = definedS "nil"
    | compileV (I.VList (v::vs)) = 
         $$ [compileV (I.VList vs),
	     compileV v,
	     definedS "cons"]
    | compileV _ = compileError "cannot compile closures directly"





  (* the symbol table tells you for every identifier its position 
       on the stack (the top of the stack is position 0)
     incrPositions takes a symbol table and increases all the positions
       by one *)

  fun incrPositions symt = 
    map (fn (ident, pos::rest) => (ident,(pos+1)::rest)) symt

  fun addIdentifier id symt = (id,[0])::(incrPositions symt)

  fun shift symt = map (fn (ident,path) => (ident,0::path)) symt

  fun maxPosition [] = ~1
    | maxPosition ((_,i::_)::symt) = Int.max (i,maxPosition symt)



  val freshName = let 
    val r = ref 0
    fun mkName prefix = let
      val i = !r
      val _ = (r := i + 1)
    in
      prefix^"_"^(Int.toString i)
    end
  in
    mkName
  end



  (* compile an expression into a sentence that produces the same value
     as the expression *)


  fun compilePrimitive2 n = let
    val name1 = freshName ":prim"
    val name2 = freshName ":prim"
  in
      $$ [S.SDefine (name2, $$ [definedS "over",
				definedS "over",
				intS 0,
				definedS "ref",
				definedS n],
		     S.SEmpty),  
	  S.SDefine (name1, $$ [definedS "over",
				quoteS name2,
				intS 1,
				definedS "closure",
				definedS "swap",
				definedS "drop"],
		     S.SEmpty),
	  quoteS name1,
	  intS 0,
	  definedS "closure"]
  end

  fun compilePrimitive1 n = let
    val name1 = freshName ":prim"
  in
    $$ [S.SDefine (name1, $$ [definedS "over",
			      definedS n],
		   S.SEmpty),
	quoteS name1,
	intS 0,
	definedS "closure"]
  end
    
  fun compilePrimitive "+" symt = compilePrimitive2 "+"
    | compilePrimitive "-" symt = compilePrimitive2 "-"
    | compilePrimitive "*" symt = compilePrimitive2 "*"
    | compilePrimitive "=" symt = compilePrimitive2 "="
    | compilePrimitive "::" symt = compilePrimitive2 "cons"
    | compilePrimitive "nil" symt = definedS "nil"
    | compilePrimitive "head" symt = compilePrimitive1 "head"
    | compilePrimitive "tail" symt = compilePrimitive1 "tail"
    | compilePrimitive n _ = definedS n


  fun compileE (I.EVal v) symt = compileV v
    | compileE (I.EFun (p,e)) symt = let
	val name = freshName ":efun"
	val env_size = 1 + maxPosition symt
      in
	$$ [S.SDefine (name, 
		       compileE e ((shift symt)@[(p,[1])]),
		       S.SEmpty),
	    quoteS name,
	    intS env_size,
	    definedS "closure"]
      end
    | compileE (I.EIf (e1,e2,e3)) symt = 
        $$ [compileE e1 symt,
	    S.SIf (compileE e2 symt,
		   compileE e3 symt,
		   S.SEmpty)]
    | compileE (I.ELet (name,e1,e2)) symt = 
        $$ [compileE e1 symt,
	    compileE e2 (addIdentifier name symt),
	    definedS "swap",
	    definedS "drop"]
    | compileE (I.ELetFun (n,p,e,body)) symt = let
	val name = freshName ":letfun"
	val env_size = 1 + maxPosition symt
      in
	$$ [S.SDefine (name, 
		       $$ [definedS "dup",
			   compileE e ((shift symt)@[(n,[1]),(p,[2])]),
			   definedS "swap",
			   definedS "drop"],
		       S.SEmpty),
	    quoteS name,
	    intS env_size,
	    definedS "closure",
	    compileE body (addIdentifier n symt),
	    definedS "swap",
	    definedS "drop"]
      end
    | compileE (I.EIdent name) symt = let
        (* if name is not found, assume it's in the global environment *)
	fun find name [] = NONE
	  | find name ((s,path)::ss) = if (name = s) then
					 SOME path
				       else find name ss
	fun ref [] = S.SEmpty
	  | ref (n::p) = $$ [intS n,
			     definedS "ref",
			     ref p]
      in
	case find name symt
	 of NONE => compilePrimitive name symt
	  | SOME (0::p) => $$ [definedS "dup", ref p]
	  | SOME (1::p) => $$ [definedS "over", ref p]
	  | SOME (n::p) => $$ [intS n, definedS "pick", ref p]
	  | _ => compileError ("cannot find "^name)
      end
    | compileE (I.EApp (e1,e2)) symt = 
        $$ [compileE e2 symt,
	    compileE e1 (incrPositions symt),
	    definedS "dup",
	    definedS "code",
	    S.SCall ($$ [definedS "swap",
			 definedS "drop",
			 definedS "swap",
			 definedS "drop"])]
    | compileE (I.EPrimCall1 e1) symt = compileError "EPrimCall1 during compilation"
    | compileE (I.EPrimCall2 e2) symt = compileError "EPrimCall2 during compilation"


  fun compileExpr expr = let
      val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
      val sent = compileE expr []
      val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
      sent
  end


  fun compileDef name params body = let
    val expr = I.makeLet name params body (I.EIdent name)
    val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
    val body = compileE expr []
    val _ = print (String.concat ["[  ", S.stringOfSentence body, " ]\n"])
  in
    body
  end


end
