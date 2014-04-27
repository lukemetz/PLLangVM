structure CompilerLLVM = struct

  structure I = InternalRepresentation

  exception Compilation of string

  fun compileError msg = raise Compilation msg
 (* compile a value into a sentence that produces that value *)
 fun filter_env sym_env = List.filter (fn x => case x of (str,reg) => not (String.substring (reg,0,1) = "@")) sym_env
  fun lookup (name:string) [] = compileError ("failed lookup for "^name)
    | lookup name ((n,sent)::env) =
        if (n = name) then
          (sent)
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
    | compileE (I.EApp (I.EApp (I.EIdent "=", e1), e2)) count sym_env cstack = opify_2 e1 e2 "eq"  count sym_env cstack 
    | compileE (I.EApp (I.EApp (I.EIdent ">", e1), e2)) count sym_env cstack = opify_2 e1 e2 "sgt" count sym_env cstack
    | compileE (I.EApp (I.EApp (I.EIdent "<", e1), e2)) count sym_env cstack = opify_2 e1 e2 "slt" count sym_env cstack 
    | compileE (I.EApp ((I.EIdent str), e)) count sym_env cstack= (case compileE e count sym_env cstack of
      (reg, count, cstack) => let 
        val func_name = lookup str sym_env 
        val str =  set_count_reg count ^
          " call %value " ^ func_name ^ " (%value* null, %value " ^ reg  ^ ")"
        in
        (count_reg (count), count + 1, cstack@[str])
                            end)

    | compileE (I.EApp(e1, e2)) count sym_env cstack = (case compileE e1 count sym_env cstack of
      (e1_reg, count, cstack) => (case compileE e2 count sym_env cstack of
      (e2_reg, count, cstack) => let
        val func_name_ptr = "%func_ptr_" ^ Int.toString count
        val func_name_env = "%func_env_" ^ Int.toString count
        val extract_func = "    " ^ func_name_ptr ^ " = "
         ^ "call %value(%value*, %value)*(%value)* @extract_func(%value " ^ e1_reg ^ ")"
        val extract_env = "    " ^ func_name_env ^ " = "
         ^ "call %value* @extract_env(%value " ^ e1_reg ^ ")"
        val call = set_count_reg count ^ " call %value " ^ func_name_ptr ^ "(%value * " ^ func_name_env ^ ", %value " ^ e2_reg ^ ")"
      in
        (count_reg count, count+1, cstack@[extract_func, extract_env, call])
      end)) 
    | compileE (I.EIf (e1, e2, e3)) count sym_env cstack= let
      val labelCount = Int.toString count
    in
      (case (compileE e1 count sym_env cstack)
       of (e1_reg, count, cstack) => (case (compileE e2 count sym_env (cstack@
                ["    %to_i8_"^labelCount^" = call i1 @extract_i1( %value " ^ e1_reg ^ ")", 
                 "    br i1 %to_i8_"^ labelCount ^ ", label %then"^labelCount^", label %else"^labelCount ^"\n",
                 "then"^labelCount^":"]))
          of (e2_reg, count, cstack) => (case (compileE e3 count sym_env (cstack@
                ["    br label %ifcont"^labelCount,"else"^labelCount^":"]))
              of (e3_reg, count, cstack) => 
                (count_reg count, count+1, cstack@
                  [
                    "    br label %ifcont"^labelCount^"\n",
                    "ifcont"^labelCount^":",
                    set_count_reg count ^ " phi %value [" ^ e2_reg ^ ", %then" ^ labelCount ^ "], [" ^ e3_reg ^ ", %else" ^ labelCount ^ "]" 
                  ]
                  ))))
    end

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
         val filtered = filter_env sym_env
         val length_env = Int.toString (List.length filtered)
         val func_name = "func_" ^ Int.toString count ^ "_" ^ Int.toString (List.length sym_env)
         val array_type = "[ " ^ length_env ^" x %value]"

         val inserts = List.foldr (fn (x,y) => case x of (name,reg) => (let
           val on_idx = Int.toString (List.length y)
           val prev_idx = if on_idx = "0" then "undef" else "%ar" ^ Int.toString ((List.length y) -1)
         in
            y@[("    %ar"^on_idx^" = insertvalue " ^ array_type ^ " " ^ prev_idx ^ ", %value "^ reg ^ ", " ^ on_idx)]
         end))
            [] filtered 
          val last_ar = "%ar" ^ Int.toString ((List.length filtered) - 1)

         val casts =   ["    %localenv = call %value* @malloc_env(i64 " ^ length_env ^ ")",
                        "    %localenv_array = bitcast %value* %localenv to " ^ array_type ^ "*",
                        "    store " ^ array_type ^ " " ^ last_ar ^ ", " ^ array_type ^ "* %localenv_array",
                        "    %localenv_ptr = bitcast " ^ array_type ^ "* %localenv_array to %value*"]
         val call = set_count_reg count ^
            "call %value @wrap_func(%value(%value*, %value)* @" ^ func_name ^ ", %value* %localenv_ptr)"
         val declare = case compileDecl func_name [arg] e1 sym_env of 
          (body, sym_env) => body
          in
            ( count_reg count, count+1, (declare::cstack)@inserts@casts@[call])
          end
    | compileE expr count sym_env cstack= compileError ("Not implemented:\n" ^ (I.stringOfExpr expr))

    (*| compileE (I.ECall (str, e::[])) count = case compileE e count of*)
      (*(strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^ " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " )"), count + 1, count + 2)*)
  and extract_env [] = []
    | extract_env sym_env =  
    let
      val array_type = "[" ^ Int.toString (List.length sym_env) ^ "x %value]"
      val extracts = List.foldr (fn (x,y) => case x of (name,reg) => 
        ("    " ^ reg ^ " = extractvalue " ^ array_type ^ " %localenv_extract, " ^ Int.toString (List.length y))::y)
         [] sym_env
      val bitcasts =
      [
        "    %localenv_extract_array = bitcast %value* %env to " ^ array_type ^ "*",
        "    %localenv_extract = load " ^ array_type ^ "* %localenv_extract_array"
      ]
    in
     bitcasts@extracts
    end

  and compileDecl sym ((argname : string)::[]) expr sym_env = 
    let
      val get_env = extract_env (filter_env sym_env)
      val header = (if sym = "main" then "define void @" else "define %value @") ^
      sym ^ "(%value* %env, %value %" ^ argname ^ ")" ^ "{" ^ (make_lines  (if sym = "main" then [] else get_env))
      val sym_env = ((sym, "@"^sym)::sym_env)
      val body = (case compileE expr 1 ((argname, "%" ^ argname)::sym_env) [header] of
        (reg, count, cstack) => (make_lines cstack) ^ "\n    ret "^ (if sym = "main" then "void" else
          ("%value " ^ reg)) ^ "\n}\n")
    in
      (body, sym_env)
  end
end
