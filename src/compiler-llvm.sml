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

  fun make_lines xs = List.foldr (fn (x,y) => (*let
    val xx = if x = "" then "" else x ^"\n"
    val yy = if y = "" then "" else y)
    in xx ^ yy end*) if x = "" then x ^ y else "\n" ^ x ^ y) "" xs
  fun count_reg count = "%"^Int.toString count
  fun set_count_reg count = "    " ^ count_reg count ^" = "
  fun compileV (I.VInt i) count = let
    val str = "    " ^ count_reg count ^ " = call %value @wrap_i32(i32 " ^ Int.toString i ^ ")"
  in
    (str, count_reg count, count+1)
  end
    | compileV _ _ = compileError "Only ints supported"

  (* compile an expression into a sentence that produces the same value
     as the expression *)

  and opify_2 e1 e2 name count sym_env= (case compileE e1 count sym_env of
      (e1_str, e1_reg, count) => (case compileE e2 count sym_env of
        (e2_str, e2_reg, count) => let
          val add_str = "    " ^ count_reg count ^ " = call %value @" ^ name ^"(%value " ^ e1_reg ^ ", %value " ^ e2_reg ^ ")"
           in
          (make_lines [e1_str, e2_str, add_str],  count_reg count, count + 1)
        end))

  and condition e1 e2 count sym_env cond = let
      val (str, reg, count) = (case compileE e1 count sym_env of
        (e1_str, e1_reg, count) => (case compileE e2 count sym_env of
          (e2_str, e2_reg, count) => (make_lines [e1_str, e2_str, 
            "    %" ^ Int.toString count ^ " = icmp "^ cond ^" %value" ^
            e1_reg ^ ", " ^ e2_reg ],
             count_reg count, count + 1)))
       in
         (str, reg, count )
       end
  and compileE (I.EVal v) count (sym_env : ((string * string) list)) = compileV v count
    | compileE (I.EIdent str) count sym_env = ("", (lookup str sym_env), count)
    | compileE (I.EApp (I.EApp (I.EIdent "+", e1), e2)) count sym_env = opify_2 e1 e2 "add" count sym_env
    | compileE (I.EApp (I.EApp (I.EIdent "-", e1), e2)) count sym_env = opify_2 e1 e2 "sub" count sym_env
    | compileE (I.EApp (I.EApp (I.EIdent "*", e1), e2)) count sym_env = opify_2 e1 e2 "mul" count sym_env
    | compileE (I.EApp (I.EApp (I.EIdent "=", e1), e2)) count sym_env = condition e1 e2 count sym_env "eq"
    | compileE (I.EApp (I.EApp (I.EIdent ">", e1), e2)) count sym_env = condition e1 e2 count sym_env "sgt"
    | compileE (I.EApp (I.EApp (I.EIdent "<", e1), e2)) count sym_env = condition e1 e2 count sym_env "slt"
    | compileE (I.EApp ((I.EIdent str), e)) count sym_env= (case compileE e count sym_env of
      (strE, reg, count) => let 
        val str = make_lines [strE, set_count_reg count ^
      " call %value @" ^ str ^ " (%value " ^ count_reg (count - 1)  ^ ")"]
        in
        (str, count_reg (count +1), count + 1)
                            end)

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
      end)))
    | compileE (I.ELet (sym1, e1, e2)) count sym_env= (case (compileE e1 count sym_env)
       of (e1_str, e1_reg, count) => let
            val new_sym_env = (sym1, e1_reg)::sym_env
         in
            (case (compileE e2 count new_sym_env)
               of (e2_str, e2_reg, count) => (make_lines [e1_str, e2_str], e2_reg,
               count))
         end)

    | compileE _ count sym_env= compileError "not supported yet"
    (*| compileE (I.ECall (str, e::[])) count = case compileE e count of*)
      (*(strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^ " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " )"), count + 1, count + 2)*)

  fun compileDecl sym ((argname : string)::[]) expr = let
    val header = (if sym = "main" then "define void @" else "define %value @") ^
    sym ^ "(%value %" ^ argname ^ ")" ^ "{"
    val body = (case compileE expr 1 ((argname, "%" ^ argname)::[]) of
      (str, reg, count) => str ^ "\n    ret "^ (if sym = "main" then "void" else
        ("%value " ^ count_reg (count -1 ))) ^ "\n}\n")
  in
    header ^ body
  end

  fun compileExpr expr = let
      (*val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])*)
      val str = (case compileE expr 1 [] of
        (str, reg, count) => let
          (*val main_end = "    call void @print (i32 %" ^ Int.toString reg ^ " )\n    ret void\n}"val *)
          val main_begin = "\ndefine void @main() {"
          val main_end = "\n    ret void\n}"
        in
          main_begin ^"\n" ^ str ^ "\n" ^ main_end
        end)
      (*val _ = print (String.concat ["[CODE\n", str, " \n]\n"])*)
  in
      str
  end
end
