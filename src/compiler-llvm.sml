structure CompilerLLVM = struct

  structure I = InternalRepresentation

  exception Compilation of string

  fun compileError msg = raise Compilation msg
 (* compile a value into a sentence that produces that value *)

  fun compileV (I.VInt i) count = ("    %" ^ Int.toString count ^ " = add i32 0, " ^ Int.toString i, count, count + 1)
    | compileV _ _ = compileError "Only ints supported"

  (* compile an expression into a sentence that produces the same value
     as the expression *)

  and opify_2 e1 e2 name count = (case compileE e1 count of
      (e1_str, e1_reg, count) => (case compileE e2 count of
        (e2_str, e2_reg, count) => (e1_str ^ "\n" ^ e2_str ^ "\n" ^
          "    %" ^ Int.toString count ^ " = " ^ name ^ " i32 %" ^ Int.toString e1_reg ^ ", %" ^ Int.toString e2_reg,
           count, count + 1)))

  and compileE (I.EVal v) count = compileV v count
    | compileE (I.EApp (I.EApp (I.EIdent "+", e1), e2)) count = opify_2 e1 e2 "add" count
    | compileE (I.EApp (I.EApp (I.EIdent "-", e1), e2)) count = opify_2 e1 e2 "sub" count
    | compileE (I.EApp (I.EApp (I.EIdent "*", e1), e2)) count = opify_2 e1 e2 "mul" count
    | compileE (I.EApp (I.EApp (I.EIdent "=", e1), e2)) count = let
      val (str, reg, count) = (case compileE e1 count of
        (e1_str, e1_reg, count) => (case compileE e2 count of
          (e2_str, e2_reg, count) => (e1_str ^ "\n" ^ e2_str ^ "\n" ^
            "    %" ^ Int.toString count ^ " = icmp eq i32 %" ^ Int.toString
            e1_reg ^ ", %" ^ Int.toString e2_reg^"\n",
             count, count + 1)))
       in
         (str, reg, count )
       end
    (*| compileE (I.EIf *)

    | compileE (I.EApp ((I.EIdent str), e)) count = (case compileE e count of
      (strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^
      " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " ) \n"), count +
      1, count + 1))

    | compileE (I.EIf (e1, e2, e3)) count = (case (compileE e1 count)
       of (e1_str, e1_reg, count) => (case (compileE e2 count)
       of (e2_str, e2_reg, count) => (case (compileE e3 count)
       of (e3_str, e3_reg, count) => let
        val labelCount = Int.toString count
        val initial = "    br i1 %" ^ (Int.toString e1_reg) ^ ", label %then"^labelCount^", label %else"^labelCount ^"\n"
        val true_block = "then"^labelCount^":\n" ^ e2_str ^
        "    br label %ifcont"^labelCount^"\n"
        val false_block = "else"^labelCount^":\n" ^ e3_str ^
        "    br label %ifcont"^labelCount^"\n"
        val final = "ifcont"^labelCount^":\n"
        val str = e1_str ^ initial ^ true_block ^ false_block ^ final

        val reg = count
        val count = count
      in
        (str, reg, count)
      end)))

    | compileE _ count = compileError "not supported yet"
    (*| compileE (I.ECall (str, e::[])) count = case compileE e count of*)
      (*(strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^ " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " )"), count + 1, count + 2)*)

  fun compileDecl sym (s::[]) expr = let
    val header = "define i32 @" ^ sym ^ "(i32 %a) {\n"
    val body = (case compileE expr 1 of
      (str, reg, count) => str)
    val footer = "    ret i32 0\n}"
  in
    header ^ body ^ "\n" ^ footer
  end

  fun compileExpr expr = let
      (*val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])*)
      val str = (case compileE expr 1 of
        (str, reg, count) => let
          (*val main_end = "    call void @print (i32 %" ^ Int.toString reg ^ " )\n    ret void\n}"val *)
          val main_begin = "\ndefine void @main() {"
          val main_end = "    ret void\n}"
        in
          main_begin ^"\n" ^ str ^ "\n" ^ main_end
        end)
      (*val _ = print (String.concat ["[CODE\n", str, " \n]\n"])*)
  in
      str
  end
end
