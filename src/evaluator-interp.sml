

(* EVALUATION VIA DIRECT INTERPRETATION *)


structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (* 
   *   Primitive operations
   *)


  fun primAdd (I.VInt i) (I.VInt j) = I.VInt (i+j)
    | primAdd _ _ = evalError "primAdd"

  fun primSub (I.VInt i) (I.VInt j) = I.VInt (i-j)
    | primSub _ _ = evalError "primSub"
		     
  fun primMul (I.VInt i) (I.VInt j) = I.VInt (i*j)
    | primMul _ _ = evalError "primMul"
		     
  fun primEq (I.VInt i) (I.VInt j) = I.VBool (i=j)
    | primEq (I.VBool b) (I.VBool c) = I.VBool (b=c)
    | primEq (I.VList l1) (I.VList l2) = I.VBool (equalLists l1 l2)
    | primEq _ _ = evalError "primEq"

  and equalLists [] [] = true
    | equalLists (v1::vs1) (v2::vs2) = 
      (case primEq v1 v2
	of I.VBool true => equalLists vs1 vs2
	 | _ => false)
    | equalLists _ _ = false

  fun primCons v (I.VList l) = I.VList (v::l)
    | primCons _ _ = evalError "primCons"

  fun primHead (I.VList (v::vs)) = v
    | primHead _ = evalError "primHead"

  fun primTail (I.VList (v::vs)) = I.VList vs
    | primTail _ = evalError "primTail"


			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = 
        if (n = name) then 
	  v
	else lookup name env 


  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v
    | eval env (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall1 (f,e1)) = f (eval env e1)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
      
  and evalApp _ (I.VClosure (n,body,env)) v = eval ((n,v)::env) body
    | evalApp _ (I.VRecClosure (f,n,body,env)) v = let
	  val new_env = [(f,I.VRecClosure (f,n,body,env)),(n,v)]@env
      in 
	  eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"
		       
  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id p expr body = let
	val f = I.VRecClosure (id, p, expr, env)
      in
        eval ((id,f)::env) body
      end

  fun execute env exp = eval env exp



  (* 
   *   initial environment
   *)

  val initialEnv = 
      [("+", I.VClosure ("a", 
			 I.EFun ("b", 
				 I.EPrimCall2 (primAdd,
					       I.EIdent "a",
					       I.EIdent "b")),
			 [])),
       ("-", I.VClosure ("a", 
			 I.EFun ("b", 
				 I.EPrimCall2 (primSub,
					       I.EIdent "a",
					       I.EIdent "b")),
			 [])),
       ("*", I.VClosure ("a", 
			 I.EFun ("b", 
				 I.EPrimCall2 (primMul,
					       I.EIdent "a",
					       I.EIdent "b")),
			 [])),
       ("::", I.VClosure ("a", 
			    I.EFun ("b", 
				    I.EPrimCall2 (primCons,
						  I.EIdent "a",
						  I.EIdent "b")),
			    [])),
       ("head", I.VClosure ("a", 
			    I.EPrimCall1 (primHead,I.EIdent "a"),
			    [])),
       ("tail", I.VClosure ("a", 
			    I.EPrimCall1 (primTail,I.EIdent "a"),
			    [])),
       ("nil", I.VList []),
       ("=", I.VClosure ("a",
			 I.EFun ("b",
				 I.EPrimCall2 (primEq,
					       I.EIdent "a",
					       I.EIdent "b")),
			 []))]



  val shellSuffix = "/interp"

  fun addDefinition env name params body = let
    val closure = eval env (I.makeLet name params body (I.EIdent name))
  in
    (name,closure)::env
  end

  fun info env = (app (fn (s,_) => (print (" "^s^"\n"))) env)

  fun stringOfValue v = I.stringOfValue v  
				 
end

