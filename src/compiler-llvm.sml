structure CompilerLLVM = struct

  structure I = InternalRepresentation

  exception Compilation of string

  fun compileError msg = raise Compilation msg
 (* compile a value into a sentence that produces that value *)

  fun lookup (name:string) [] = compileError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) =
        if (n = name) then
	  sent
	else lookup name env

  fun make_lines xs = List.foldr (fn (x,y) => 
    if x = "" then x ^ y else "\n" ^ x ^ y) "" xs
  fun count_reg count = "%"^Int.toString count
  fun set_count_reg count = "    " ^ count_reg count ^" = "
  fun compileV (I.VInt i) count cstack = let
    val str = "    " ^ count_reg count ^ " = call %value @wrap_i32(i32 " ^ Int.toString i ^ ")"
  in
    (count_reg count, count+1, cstack@[str])
  end
    | compileV _ _ _= compileError "Only ints supported"

  (* compile an expression into a sentence that produces the same value
     as the expression *)

  and opify_2 e1 e2 name count sym_env cstack = (case compileE e1 count sym_env cstack of
      (e1_reg, count, cstack) => (case compileE e2 count sym_env cstack of
        (e2_reg, count, cstack) => let
          val add_str = "    " ^ count_reg count ^ " = call %value @" ^ name ^"(%value " ^ e1_reg ^ ", %value " ^ e2_reg ^ ")"
           in
          (count_reg count, count + 1, cstack@[add_str])
        end))

  and condition e1 e2 count sym_env cstack cond =
      (case compileE e1 count sym_env cstack of
        (e1_reg, count, cstack) => (case compileE e2 count sym_env cstack of
          (e2_reg, count, cstack) => 
            (count_reg count, count + 1, cstack@["    %" ^ Int.toString count ^ " = icmp "^ cond ^" %value" ^ e1_reg ^ ", " ^ e2_reg ])))

  and compileE (I.EVal v) count sym_env cstack = compileV v count cstack
    | compileE (I.EIdent str) count sym_env cstack = ((lookup str sym_env), count, cstack)
    | compileE (I.EApp (I.EApp (I.EIdent "+", e1), e2)) count sym_env cstack = opify_2 e1 e2 "add" count sym_env cstack
    | compileE (I.EApp (I.EApp (I.EIdent "-", e1), e2)) count sym_env cstack = opify_2 e1 e2 "sub" count sym_env cstack
    | compileE (I.EApp (I.EApp (I.EIdent "*", e1), e2)) count sym_env cstack = opify_2 e1 e2 "mul" count sym_env cstack
    | compileE (I.EApp (I.EApp (I.EIdent "=", e1), e2)) count sym_env cstack = condition e1 e2 count sym_env cstack "eq"
    | compileE (I.EApp (I.EApp (I.EIdent ">", e1), e2)) count sym_env cstack = condition e1 e2 count sym_env cstack "sgt"
    | compileE (I.EApp (I.EApp (I.EIdent "<", e1), e2)) count sym_env cstack = condition e1 e2 count sym_env cstack "slt"
    | compileE (I.EApp ((I.EIdent str), e)) count sym_env cstack= (case compileE e count sym_env cstack of
      (reg, count, cstack) => let 
        val func_name = lookup str sym_env
        val str =  set_count_reg count ^
      " call %value " ^ func_name ^ " (%value " ^ count_reg (count - 1)  ^ ")"
        in
        (count_reg (count +1), count + 1, cstack@[str])
                            end)
(*
    | compileE (I.EIf (e1, e2, e3)) count sym_env= (case (compileE e1 count sym_env)
       of (e1_str, e1_reg, count) => (case (compileE e2 count sym_env)
       of (e2_str, e2_reg, count) => (case (compileE e3 count sym_env)
       of (e3_str, e3_reg, count) => let
        val labelCount = Int.toString count
        val initial = "    br i1 " ^ (e1_reg) ^ ", label %then"^labelCount^", label %else"^labelCount ^"\n"
        val true_block = "then"^labelCount^":\n" ^ e2_str ^
        "    br label %ifcont"^labelCount^"\n"
        val false_block = "else"^labelCount^":\n" ^ e3_str ^
        "    br label %ifcont"^labelCount^"\n"
        val final = "ifcont"^labelCount^":\n"
        val str = e1_str ^ initial ^ true_block ^ false_block ^ final

        val count = count
      in
        (str, count_reg count, count)
      end)))*)
    | compileE (I.ELet (sym1, e1, e2)) count sym_env cstack= (case (compileE e1 count sym_env cstack)
       of (e1_reg, count, cstack) => let
            val new_sym_env = (sym1, e1_reg)::sym_env
         in
            (case (compileE e2 count new_sym_env cstack)
               of (e2_reg, count, cstack) => (e2_reg, count, cstack))
         end)

    | compileE (I.ELetFun (func_name, arg_name, e1, e2)) count sym_env cstack= let
      val efun_expr = I.EFun(arg_name, e1)
      in
      (case (compileE efun_expr count sym_env cstack) of
        (efun_reg, count, cstack) => let
          val new_sym_env = (func_name, efun_reg)::sym_env
        in
          (case (compileE e2 count new_sym_env cstack) of
            (e2_reg, count, cstack) =>
              (e2_reg, count+1, cstack)
            )
        end)
    end



    | compileE (I.EFun (arg, e1)) count sym_env cstack =
    (*(case (compileE e1 count sym_env cstack)*)
       (*of (e1_reg, count, cstack) =>*)
       let
         val func_name = "func_" ^ Int.toString count
          val _ = print ("&&&&&&&&&&======"^arg^"\n\n")
         val call = set_count_reg count ^
            "call %value @wrap_func(%value(%value)* @" ^ func_name ^ ")"
         val declare = case compileDecl func_name [arg] e1 sym_env of 
          (body, sym_env) => body
          in
            ( "@" ^ func_name, count+1, (declare::cstack)@[call])
          end



    | compileE _ count sym_env cstack= compileError "not supported yet"
    (*| compileE (I.ECall (str, e::[])) count = case compileE e count of*)
      (*(strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^ " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " )"), count + 1, count + 2)*)

  and compileDecl sym ((argname : string)::[]) expr sym_env= let
    val header = (if sym = "main" then "define void @" else "define %value @") ^
    sym ^ "(%value %" ^ argname ^ ")" ^ "{"
    val sym_env = (sym, "@"^sym)::sym_env
    val _ = print ("&&&&&&&&&&"^argname^"\n\n")
    val body = (case compileE expr 1 ((argname, "%" ^ argname)::sym_env) [header] of
      (reg, count, cstack) => (make_lines cstack) ^ "\n    ret "^ (if sym = "main" then "void" else
        ("%value " ^ reg)) ^ "\n}\n")
  in
    (body, sym_env)
  end
end
