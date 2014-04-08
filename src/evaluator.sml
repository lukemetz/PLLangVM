

(* EVALUATION VIA COMPILATION TO INTERPRETED STACK LANGUAGES *)



structure Evaluator = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation
  structure C = Compiler
  structure Pr = Primitives



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) = 
        if (n = name) then 
	  sent
	else lookup name env 




  fun execute' env S.SEmpty stack = stack
    | execute' env (S.SSequence (S.WInt i,ws)) stack = 
        execute' env ws ((S.VInt i)::stack)
    | execute' env (S.SSequence (S.WPrim (_,prim), ws)) stack = 
        execute' env ws (prim stack)
    | execute' env (S.SSequence (S.WDefined w, ws)) stack = let
	val stack = execute' env (lookup w env) stack
      in
        execute' env ws stack
      end
    | execute' env (S.SIf (trueS,falseS,thenS)) (v::stack) = let
        val stack = execute' env (case v 
			      of S.VInt 0 => falseS
			       | _ => trueS) stack
      in
	execute' env thenS stack
      end
    | execute' env (S.SQuote (w,ws)) stack = let
        val stack = (S.VCode (lookup w env))::stack
      in
        execute' env ws stack
      end
    | execute' env (S.SCall ws) ((S.VCode st)::stack) = let
        val stack = execute' env st stack
      in
        execute' env ws stack
      end
    | execute' env (S.SDefine (w,wsdef,st)) stack = 
        execute' ((w,wsdef)::env) st stack
    | execute' _ _ _ = evalError "cannot execute sentence"



  fun execute env expr = let
    val sent = C.compileExpr expr
    val stack = execute' env sent []
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
    fun process (n,prim) = (n,S.SSequence (S.WPrim (n,prim),S.SEmpty))
  in
    map process 
       [("+", Pr.primAdd),
        ("*", Pr.primMul),
 	("-", Pr.primSub),
 	("=", Pr.primEq),
  ("<", Pr.primLess),
  (">", Pr.primGreater),
	("mod", Pr.primMod ),
        ("dup", Pr.primDup),
	("swap", Pr.primSwap),
	("over", Pr.primOver),
	("rot", Pr.primRot),
	("pick", Pr.primPick),
	("ref", Pr.primRef),
	("closure", Pr.primClosure),
	("code", Pr.primCode),
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
  end

  val shellSuffix = ""


  fun addDefinition env name params body = let
    val sent = C.compileDef name params body
  in
    (name,sent)::env
  end

  fun info env = (app (fn (s,_) => (print (" "^s^"\n"))) env)

  fun stringOfValue v = S.stringOfValue v  
				 
end
