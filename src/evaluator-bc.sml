

(* EVALUATION VIA COMPILATION TO STACK MACHINE BYTE CODES *)


structure Evaluator = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation
  structure C = Compiler
  structure Pr = Primitives


  exception Evaluation of string

  fun evalError msg = raise Evaluation msg



  (* Stack machine bytecodes *)

  type code_address = int

  datatype instruction = Value of S.value
		       | PushCode of code_address
		       | Jump
		       | JumpIfZero
		       | Primitive of string * (S.value list -> S.value list)
		       | PushCodeValue

  fun stringOfInstruction (Value v) = 
         String.concat ["PUSHV[",S.stringOfValue(v),"]"]
    | stringOfInstruction (PushCode s) = 
         String.concat ["PUSHC[",Int.toString s,"]"]
    | stringOfInstruction (PushCodeValue) = 
         "PUSHCV"
    | stringOfInstruction (Jump) = 
         "JUMP"
    | stringOfInstruction (JumpIfZero) =
         "JUMPZ"
    | stringOfInstruction (Primitive (n,_)) = 
         String.concat ["PRIM[", n, "]"]

  fun printInstruction offset (i,instr) = let
    fun fill4 n = let 
      val s = "0000"^(Int.toString n)
    in
      String.extract (s,(String.size s)-4,NONE)
    end
  in
    print (String.concat ["  ", fill4 (i+offset), ":  ",
			  stringOfInstruction instr,
			  "\n"])
  end

			 


  fun execute_bc code pos cstack stack = let
    fun get_bc code pos = Vector.sub (code, pos)
    fun exec (Value v) cstack stack =
  	  execute_bc code (pos+1) cstack (v::stack)
      | exec (PushCode s) cstack stack = 
	  execute_bc code (pos+1) (s::cstack) stack
      | exec PushCodeValue cstack ((S.VCode s)::stack) = 
	  execute_bc code (pos+1) (s::cstack) stack
      | exec (Primitive (_,p)) cstack stack =
	  execute_bc code (pos+1) cstack (p stack)
      | exec Jump [] stack = stack
      | exec Jump (s::cstack) stack = 
          execute_bc code s cstack stack
      | exec JumpIfZero (s::cstack) ((S.VInt 0)::stack) = 
          execute_bc code s cstack stack
      | exec JumpIfZero (s::cstack) (_::stack) = 
          execute_bc code (pos+1) cstack stack
      | exec _ _ _ = evalError "Cannot execute bytecode"
  in
    exec (get_bc code pos) cstack stack
  end




  (* compile into stack machine bytecodes *)

  fun lookup (name:string) [] = C.compileError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) = 
        if (n = name) then 
	  sent
	else lookup name env 


  fun create_bc env S.SEmpty pos = 
         [Jump]
    | create_bc env (S.SSequence (S.WInt i,ws)) pos = 
         [Value (S.VInt i)]
    	  @(create_bc env ws (pos + 1))
    | create_bc env (S.SSequence (S.WPrim (n,p),ws)) pos = 
         [Primitive (n, p)]
  	  @(create_bc env ws (pos + 1))
    | create_bc env (S.SSequence (S.WDefined w, S.SEmpty)) pos = let
          val code_addr = lookup w env
        in   
          [PushCode code_addr, 
	   Jump]
        end
    | create_bc env (S.SSequence (S.WDefined w, ws)) pos = let
          val code_addr = lookup w env
        in   
          [PushCode (pos+3),
	   PushCode code_addr, 
	   Jump]
	   @(create_bc env ws (pos + 3))
        end
    | create_bc env (S.SIf (trueS,falseS,S.SEmpty)) pos = let
	  val true_code = create_bc env trueS (pos + 2)
	  val false_start = pos + 2 + length true_code
	  val false_code = create_bc env falseS false_start
      in
          [PushCode false_start,
	   JumpIfZero]
	   @true_code
	   @false_code
      end
    | create_bc env (S.SIf (trueS,falseS,thenS)) pos = let
	  val true_code = create_bc env trueS (pos + 3)
	  val false_start = pos + 3 + length true_code
	  val false_code = create_bc env falseS false_start
	  val then_start = false_start + length false_code
	  val then_code = create_bc env thenS then_start
      in
          [PushCode then_start,
	   PushCode false_start,
	   JumpIfZero]
	   @true_code
	   @false_code
	   @then_code
      end
    | create_bc env (S.SQuote (w,ws)) pos = let
	val code_addr = lookup w env
      in
        [Value (S.VCode code_addr)]
	  @(create_bc env ws (pos + 1))
      end
    | create_bc env (S.SCall S.SEmpty) pos = 
        [PushCodeValue,
	 Jump]
    | create_bc env (S.SCall ws) pos = 
        [PushCode (pos+3),
	 PushCodeValue,
	 Jump]
	  @(create_bc env ws (pos+3))
    | create_bc env (S.SDefine (w,wsdef,S.SEmpty)) pos = let
        val def_code = create_bc ((w,pos+2)::env) wsdef (pos + 1)
      in
	[Jump]
	  @def_code
      end
    | create_bc env (S.SDefine (w,wsdef,ws)) pos = let
        val def_code = create_bc ((w,pos+2)::env) wsdef (pos + 2)
	val rest_code = create_bc ((w,pos+2)::env) ws (pos + 2 + length def_code)
      in
	[PushCode (pos + 2 + length def_code),
	 Jump]
	  @def_code
	  @rest_code
      end



  fun compileSentence (code,env) sent = let
    (* put the compiled code at the end of the current code *)
    val pos = Vector.length code
    val _ = print (String.concat ["[compiling sentence ", S.stringOfSentence sent, "]\n"])
    val bcs = create_bc env sent pos
    val vec_bcs = Vector.fromList bcs
    val _ = Vector.appi (printInstruction pos) vec_bcs
  in
    (Vector.concat [code,vec_bcs],
     pos)
  end


  fun execute (code,env) expr = let
    val sent = C.compileExpr expr
    val (code,bcs_pos) = compileSentence (code,env) sent
    val stack = execute_bc code bcs_pos [] []
  in
    case stack
     of [] => evalError "Expression returned no value!"
      | (v::[]) => v
      | (v::_) => (print "Warning - stack not empty\n"; v)
  end




  (* 
   *   Initial environment 
   *)

  val initialEnv = let
    fun makePrim (n,p) = [Primitive (n,p), Jump]
    fun makeEntry ((n,_),pos) = (n,pos)
    val primitives = [ ("+", Pr.primAdd),
		       ("*", Pr.primMul),
 		       ("-", Pr.primSub),
		       ("mod", Pr.primMod),
		       ("dup", Pr.primDup),
		       ("swap", Pr.primSwap),
		       ("over", Pr.primOver),
		       ("rot", Pr.primRot),
		       ("pick", Pr.primPick),
		       ("closure", Pr.primClosure),
		       ("ref", Pr.primRef),
		       ("code", Pr.primCode),
		       ("=", Pr.primEq),
		       ("0=", Pr.primZeroEq),
		       ("0>", Pr.primZeroGt),
		       ("drop", Pr.primDrop),
		       ("cons", Pr.primCons),
		       ("head", Pr.primHead),
		       ("tail", Pr.primTail),
		       ("nil=", Pr.primNilEq),
		       ("nil", Pr.primNil),
		       ("empty-stack", Pr.primEmptyStack),
		       ("show-stack", Pr.primShowStack)
		     ] 
    val code = Vector.fromList (foldr (fn (x,r) => (makePrim x)@r) [] primitives)
    val env =  ListPair.map makeEntry (primitives,
     				       List.tabulate (length primitives,
						      fn i => 2*i))
  in
    (code,env)
  end


  val shellSuffix = "/bytecodes"

  fun addDefinition (code,env) name params body = let
    val sent = C.compileDef name params body
    (*   hack to get recursion to work
     *   this only works if bcs_pos is next free available memory location
     *   (there's a check to fail if that's not the case...)
     *)
    val pos = Vector.length code
    val env' = (name,pos)::env
    val (code,bcs_pos) = compileSentence (code,env') sent
  in
      if pos = bcs_pos then
        (code,env')
      else evalError "definition not compiled at expected position"
  end


  fun info (code,env) = 
    Vector.appi (printInstruction 0) code

  val stringOfValue = S.stringOfValue
				 
end
