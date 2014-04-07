
structure InternalRepresentation = struct

  datatype value = VInt of int
  	         | VBool of bool
		 | VList of value list
		 | VClosure of string * expr * (string * value) list
		 | VRecClosure of string * string * expr * (string * value) list
			    
  and expr = EVal of value
	   | EFun of string * expr
	   | EIf of expr * expr * expr
	   | ELet of string * expr * expr
	   | ELetFun of string * string * expr * expr
	   | EIdent of string
	   | EApp of expr * expr
           | EPrimCall1 of (value -> value) * expr
           | EPrimCall2 of (value -> value -> value) * expr * expr

  fun makeLet name (p::params) expr body = let
        fun makeEFuns [] = expr
	  | makeEFuns (p::ps) = EFun (p,makeEFuns ps)
      in
        ELetFun (name,p,makeEFuns params,body)
      end
    | makeLet name [] expr body = ELet (name,expr,body)


  fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strS s = "\""^s^"\""
    fun strV (VInt i) = $ ["VInt ",Int.toString i]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VList l) = $ ["VList [", $+ (map strV l), "]"]
      | strV (VClosure (n,e,_)) = $ ["VClosure (", n, ",", strE e, ")"]
      | strV (VRecClosure (f,n,e,_)) = $ ["VRecClosure (", f, ",",n, ",", strE e, ")"]
    and strE (EVal v) = strCon "EVal" strV [v]
      | strE (EFun (n,e)) = $ ["EFun (", n, ",", strE e, ")"]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" strE [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
      | strE (ELetFun (n,p,e1,e2)) = $ ["ELetFun (",strS n,",",
					strS p,",",
					strE e1,",",strE e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (EApp (e1,e2)) = strCon "EApp" strE [e1,e2]
      | strE (EPrimCall1 (f,e1)) = strCon "EPrimCall2" strE [e1]
      | strE (EPrimCall2 (f,e1,e2)) = strCon "EPrimCall2" strE [e1,e2]
  in
    strE e
  end

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VClosure (n,e,_)) = 
        String.concat ["<function (", n, ",", stringOfExpr e,")>"]
    | stringOfValue (VRecClosure (f,n,e,_)) = 
        String.concat ["<function ", f, " (", n, ",", stringOfExpr e,")>"]
    | stringOfValue (VList l) = 
        String.concat ["[", 
		       String.concatWith "," (map stringOfValue l), 
		       "]"]

  fun printValue v = (print (stringOfValue v);
		      print "\n")
		       
end
