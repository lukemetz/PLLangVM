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
    | appendS (S.SWhile (s1,s2)) s3 = S.SWhile (s1,appendS s2 s3)

  fun appendSs [] = S.SEmpty
    | appendSs (s1::ss) = appendS s1 (appendSs ss)

  fun wordS (w) = S.SSequence (w,S.SEmpty)



  (* compile a value into a sentence that produces that value *)

  fun compileV (I.VInt i) = wordS (S.WInt i)
    | compileV (I.VBool true) = wordS (S.WInt 1)
    | compileV (I.VBool false) = wordS (S.WInt 0)
    | compileV (I.VList []) = wordS (S.WDefined "nil")
    | compileV (I.VList (v::vs)) = 
         appendSs [compileV (I.VList vs),
		   compileV v,
		   wordS (S.WDefined "cons")]


  (* the symbol table tells you for every identifier its position 
       on the stack (the top of the stack is position 0)
     incrPositions takes a symbol table and increases all the positions
       by one *)

  fun incrPositions symt = 
    map (fn (ident,pos) => (ident,pos+1)) symt

  fun addIdentifier id symt = 
    (id,0)::(incrPositions symt)


  (* compile an expression into a sentence that produces the same value
     as the expression *)

  fun compilePrim1 pr e symt = 
    appendSs [compileE e symt,
	      wordS (S.WDefined pr)]

  and compilePrim2 pr e1 e2 symt = 
      appendSs [compileE e1 symt,
		compileE e2 (incrPositions symt),
		wordS (S.WDefined pr)]

  and compileE (I.EVal v) symt = compileV v
    | compileE (I.EIf (e1,e2,e3)) symt = 
         appendSs [compileE e1 symt,
		   S.SIf (compileE e2 symt,
			  compileE e3 symt,
			  S.SEmpty)]
    | compileE (I.ELet (name,e1,e2)) symt = 
         appendSs [compileE e1 symt,
		   compileE e2 (addIdentifier name symt),
		   wordS (S.WDefined "swap"),
		   wordS (S.WDefined "drop")]
    | compileE (I.EIdent name) symt = let
	fun find name [] = compileError ("Unbound identifier - "^name)
	  | find name ((s,i)::ss) = if (name = s) then
				      i
				    else find name ss
      in
	case find name symt
	 of 0 => wordS (S.WDefined "dup")
	  | 1 => wordS (S.WDefined "over")
	  | n => appendSs [wordS (S.WInt n),
			   wordS (S.WDefined "pick")]
      end
    | compileE (I.EAdd (e1,e2)) symt = compilePrim2 "+" e2 e1 symt
    | compileE (I.ESub (e1,e2)) symt = compilePrim2 "-" e2 e1 symt
    | compileE (I.EMul (e1,e2)) symt = compilePrim2 "*" e2 e1 symt
    | compileE (I.EEq (e1,e2)) symt = compilePrim2 "=" e2 e1 symt
    | compileE (I.ECons (e1,e2)) symt = compilePrim2 "cons" e2 e1 symt
    | compileE (I.EHead e1) symt = compilePrim1 "head" e1 symt
    | compileE (I.ETail e1) symt = compilePrim1 "tail" e1 symt
    | compileE (I.ECall (name,es)) symt = let
	fun push [] = (S.SEmpty,symt)
	  | push (e::es)  = let
	      val (sent,symt) = push es
	    in
	      (appendS sent (compileE e symt),
	       incrPositions symt)
	    end
	val (sent, _) = push es
      in
	appendSs ([sent,
		   wordS (S.WDefined name)]
		  @(map (fn _ => (appendS (wordS (S.WDefined "swap"))
					  (wordS (S.WDefined "drop")))) es))
      end


  fun compileExpr expr = let
      val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
      val sent = compileE expr []
      val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
      sent
  end


  fun compileDef name params expr = let
    val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
    val symt = foldr (fn (id,r) => addIdentifier id r) [] params
    val body = compileE expr symt
    val _ = print (String.concat ["[  ", S.stringOfSentence body, " ]\n"])
  in
    body
  end



end
