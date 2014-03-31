structure CompilerLLVM = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation


  exception Compilation of string

  fun compileError msg = raise Compilation msg
 (* compile a value into a sentence that produces that value *)

  fun compileV (I.VInt i) count = ("%" ^ Int.toString count ^ " = add i32 0, " ^ Int.toString i, count, count + 1)
    | compileV _ _ = compileError "Only ints supported"

  (* compile an expression into a sentence that produces the same value
     as the expression *)

  and opify_2 e1 e2 name count = (case compileE e1 count of 
      (e1_str, e1_reg, count) => (case compileE e2 count of 
        (e2_str, e2_reg, count) => (e1_str ^ "\n" ^ e2_str ^ "\n" ^ 
          "%" ^ Int.toString count ^ "= " ^ name ^ " i32 %" ^ Int.toString e1_reg ^ ", %" ^ Int.toString e2_reg,
           count, count + 1)))


  and compileE (I.EVal v) count = compileV v count
    | compileE (I.EAdd (e1,e2)) count = opify_2 e1 e2 "add" count 
    | compileE (I.ESub (e1, e2)) count = opify_2 e1 e2 "sub" count
    | compileE (I.EMul (e1, e2)) count = opify_2 e1 e2 "mul" count

  fun compileExpr expr = let
      (*val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])*)
      val str = (case compileE expr 1 of
        (str, reg, count) => let
          val main_end = "call void @print (i32 %" ^ Int.toString reg ^ " )\nret void\n\n}"
          val boiler = "@printFmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\ndefine void @print(i32 %a) {\nentry:\n%0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @printFmt, i32 0, i32 0), i32 %a)\nret void\n}\ndeclare i32 @printf(i8*, ...)\n"
          val main_begin = "\ndefine void @main() {"
        in
          boiler ^"\n\n" ^ main_begin ^"\n" ^ str ^ "\n" ^ main_end
        end)
      (*val _ = print (String.concat ["[CODE\n", str, " \n]\n"])*)
  in
      str
  end


end
