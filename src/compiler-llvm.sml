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
    | compileE (I.EApp ((I.EIdent str), e)) count = case compileE e count of
      (strE, reg, count) => ((strE ^ "\n    " ^ "%" ^ (Int.toString (count)) ^
      " = call i32 @" ^ str ^ " (i32 %" ^ (Int.toString (reg))  ^ " )"), count + 1, count + 2)
    (*| compileE (I.EApp (I.EApp (I.EIdent oper, e1), e2)) count = opify_2 e1 e2 oper count*)
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
