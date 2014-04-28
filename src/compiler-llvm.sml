structure CompilerLLVM = struct
  structure I = InternalRepresentation

(*Compilation Exception Declaration and Helper*)
exception Compilation of string
fun compileError msg = raise Compilation msg
 
(*Filter functions out of symbol environemnt*)
fun filter_env sym_env = List.filter (fn x => case x of (str,reg) => not (String.substring (reg,0,1) = "@")) sym_env

(*Joins lines of code with new lines to output to llvm file*)
fun make_lines xs = List.foldr (fn (x,y) => if x = "" then x ^ y else "\n" ^ x ^ y) "" xs

(*Int.toString helper*)
fun itos i = Int.toString i

(*Helper to add % in front of count for temporary variables*)
fun count_reg count = "%" ^ (itos count)

(*Helper to set temp variables*)
fun set_count_reg count = "    " ^ (count_reg count) ^" = "

(*Symbol Environment Lookup*)
fun lookup (name:string) [] = compileError ("failed lookup for "^name)
  | lookup name ((n,sent)::env) =
    if (n = name) 
      then
        sent
      else 
        lookup name env 

(*Original Decl compilation*)
fun compileDecl sym params expr sym_env = 
  let
    val argname = (hd params)
    val expr = make_curry (tl params) expr
    val get_env = extract_env (filter_env sym_env)
    val sym_env = ((sym, "@"^sym)::sym_env)
    val header = 
        (if sym = "main" 
          then 
            "define void @"
          else 
            "define %value @"
        )
        ^ sym ^ "(%value* %env, %value %" ^ argname ^ ")"
        ^ "{" ^ 
        (make_lines (if sym = "main" then [] else get_env))

    val body = 
        (case compileE expr 1 ((argname, "%" ^ argname)::sym_env) [header] 
          of (reg, count, cstack) => 
            (make_lines cstack) ^ "\n    ret "
            ^ (if sym = "main" then "void" else ("%value " ^ reg)) ^ "\n}\n"
        )
  in
    (body, sym_env)
  end


(*Compile Expression body*)
and compileE (I.EVal v) count sym_env cstack                               = compileV v count cstack
  | compileE (I.EIdent str) count sym_env cstack                           = ((lookup str sym_env), count, cstack)
  | compileE (I.EApp (I.EApp (I.EIdent "+", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "add" count sym_env cstack
  | compileE (I.EApp (I.EApp (I.EIdent "-", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "sub" count sym_env cstack
  | compileE (I.EApp (I.EApp (I.EIdent "*", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "mul" count sym_env cstack
  | compileE (I.EApp (I.EApp (I.EIdent "=", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "eq"  count sym_env cstack 
  | compileE (I.EApp (I.EApp (I.EIdent ">", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "sgt" count sym_env cstack
  | compileE (I.EApp (I.EApp (I.EIdent "<", e1), e2)) count sym_env cstack = compileE_oper e1 e2 "slt" count sym_env cstack 
  | compileE (I.EApp ((I.EIdent str), e)) count sym_env cstack             = compileE_call str e count sym_env cstack
  | compileE (I.EApp(e1, e2)) count sym_env cstack                         = compileE_callfunc e1 e2 count sym_env cstack
  | compileE (I.EIf (e1, e2, e3)) count sym_env cstack                     = compileE_if e1 e2 e3 count sym_env cstack
  | compileE (I.ELet (sym, e1, e2)) count sym_env cstack                   = compileE_let sym e1 e2 count sym_env cstack 
  | compileE (I.ELetFun (func, arg, e1, e2)) count sym_env cstack          = compileE_letfun func arg e1 e2 count sym_env cstack
  | compileE (I.EFun (arg, e1)) count sym_env cstack                       = compileE_fun arg e1 count sym_env cstack
  | compileE expr count sym_env cstack                                     = compileError ("Not implemented:\n" ^ (I.stringOfExpr expr))

(*Compile Value to create i32*)
and compileV (I.VInt i) count cstack = 
      let
        val str = "    " ^ count_reg count ^ " = call %value @wrap_i32(i32 " ^ itos i ^ ")"
      in
        (count_reg count, count+1, cstack@[str])
      end
  | compileV _ _ _= compileError "Only ints supported"

(* Handles 2 argument operations (basic operations)*)
and compileE_oper e1 e2 name count sym_env cstack = 
  (case compileE e1 count sym_env cstack
    of (e1_reg, count, cstack) => 
      (case compileE e2 count sym_env cstack 
        of (e2_reg, count, cstack) => 
          let
            val add_str = (set_count_reg count) ^ "call %value @" ^ name ^"(%value " ^ e1_reg ^ ", %value " ^ e2_reg ^ ")"
          in
            (count_reg count, count + 1, cstack@[add_str])
        end
      )
  )

(* Extracts function call *)
and compileE_callfunc e1 e2 count sym_env cstack = 
  (case compileE e1 count sym_env cstack
    of (e1_reg, count, cstack) => 
      (case compileE e2 count sym_env cstack
        of (e2_reg, count, cstack) => 
          let
            val func_ptr = "%func_ptr_" ^ (itos count)
            val func_env = "%func_env_" ^ (itos count)
            val extract_func = "    " ^ func_ptr ^ " = "
                ^ "call %value(%value*, %value)*(%value)* @extract_func(%value " ^ e1_reg ^ ")"
            val extract_env  = "    " ^ func_env ^ " = "
                ^ "call %value* @extract_env(%value " ^ e1_reg ^ ")"
            val call = set_count_reg count ^ " call %value " ^ func_ptr ^ "(%value * " ^ func_env ^ ", %value " ^ e2_reg ^ ")"
          in
            (count_reg count, count + 1, cstack@[extract_func, extract_env, call])
          end
      )
  )

(*Calls the function extracted *)
and callFunc (I.EIdent str) e2 count sym_env cstack = 
  (case compileE e2 count sym_env cstack
    of (reg, count, cstack) => 
      let
        val func_name = lookup str sym_env
        val str =  set_count_reg count ^
          " call %value " ^ func_name ^ " (%value* null, %value " ^ reg  ^ ")"
      in
        (count_reg (count), count + 1, cstack@[str])
      end
  )

(* (I.EApp ((I.EIdent str), e)) *)
and compileE_call str e count sym_env cstack = 
  (case compileE e count sym_env cstack 
    of (reg, count, cstack) => 
      let
        val func_name = lookup str sym_env
        val first_letter = String.substring (func_name, 0, 1)
      in
        if first_letter = "@" 
          then
            callFunc (I.EIdent str) e count sym_env cstack
          else
            compileE_callfunc (I.EIdent str) e count sym_env cstack
      end
  )
(*I.EIf (e1, e2, e3)*)
and compileE_if e1 e2 e3 count sym_env cstack = 
  let
    val labelCount = itos count
    val to_label = "%to_i8_" ^ labelCount
    val br_label = "    br label %ifcont" ^ labelCount
  in
    (case (compileE e1 count sym_env cstack)
      of (e1_reg, count, cstack) => 
        (case (compileE e2 count sym_env (cstack @
                [
                  "    " ^ to_label ^ " = call i1 @extract_i1( %value " ^ e1_reg ^ ")", 
                  "    br i1 " ^ to_label ^ ", label %then" ^ labelCount ^ ", label %else" ^ labelCount,
                  "then" ^ labelCount ^ ":"
                ]))
          of (e2_reg, count, cstack) => 
            (case (compileE e3 count sym_env (cstack@[br_label, "else" ^ labelCount ^ ":"]))
              of (e3_reg, count, cstack) => 
                (count_reg count, count+1, cstack@
                  [
                    br_label,
                    "ifcont" ^ labelCount ^ ":",
                    set_count_reg count ^ " phi %value [" ^ e2_reg ^ ", %then" ^ labelCount ^ "], [" ^ e3_reg ^ ", %else" ^ labelCount ^ "]" 
                  ]
                )
            )
        )
    )
  end

(*(I.ELet (sym, e1, e2))*)
and compileE_let sym e1 e2 count sym_env cstack = 
  (case (compileE e1 count sym_env cstack)
    of (e1_reg, count, cstack) => 
      let
        val new_sym_env = (sym, e1_reg)::sym_env
      in
        (case (compileE e2 count new_sym_env cstack)
          of (e2_reg, count, cstack) => 
            (e2_reg, count, cstack)
        )
      end
  )

(*(I.ELetFun (func, arg, e1, e2))*)
and compileE_letfun func arg e1 e2 count sym_env cstack =
  let
    val efun_expr = I.EFun(arg, e1)
  in
    (case (compileE efun_expr count sym_env cstack)
      of (efun_reg, count, cstack) => 
        let
          val new_sym_env = (func, efun_reg)::sym_env
        in
          (case (compileE e2 count new_sym_env cstack) 
            of (e2_reg, count, cstack) =>
              (e2_reg, count + 1, cstack)
          )
        end
    )
  end

(*(I.EFun (arg, e1))*)
and compileE_fun arg e1 count sym_env cstack = 
  let
    val filtered = filter_env sym_env
    val length_env = itos (List.length filtered)
    val func_name = "func_" ^ (itos count) ^ "_" ^ itos (List.length sym_env)
    val array_type = "[ " ^ length_env ^" x %value]"
    val ar_base = "%ar_" ^ (itos count) ^ "_"
    val last_ar = ar_base ^ itos ((List.length filtered) - 1)
    val localenv = "%localenv_"^ (itos count)
    val declare = case compileDecl func_name [arg] e1 sym_env of 
          (body, sym_env) => body
    val casts = 
      [
        "    "^ localenv ^ " = call %value* @malloc_env(i64 " ^ length_env ^ ")",
        "    " ^ localenv ^ "_array = bitcast %value* " ^ localenv ^ " to " ^ array_type ^ "*",
        "    store " ^ array_type ^ " " ^ last_ar ^ ", " ^ array_type ^ "* "^ localenv ^ "_array",
        "    " ^ localenv ^"_ptr = bitcast " ^ array_type ^ "* " ^ localenv ^ "_array to %value*"
      ]
    val call = set_count_reg count
            ^ "call %value @wrap_func(%value(%value*, %value)* @" 
            ^ func_name ^ ", %value* " ^ localenv ^ "_ptr)"
    val inserts = 
      List.foldr (fn (x,y) => 
        (case x of (name,reg) => 
          (let
            val on_idx = itos (List.length y)
            val prev_idx = 
              if on_idx = "0" 
                then 
                  "undef" 
                else 
                  ar_base ^ itos ((List.length y) -1)
          in
            y@[("    " 
                ^ ar_base ^ on_idx ^ " = insertvalue " 
                ^ array_type ^ " " ^ prev_idx ^ ", %value " ^ reg ^ ", " ^ on_idx)]
          end)
        )) [] filtered 
  in
    ( count_reg count, count + 1, (declare::cstack)@inserts@casts@[call])
  end

(*Extracts the environment*)
and extract_env [] = []
  | extract_env sym_env =  
    let
      val array_type = "[" ^ itos (List.length sym_env) ^ "x %value]"
      val bitcasts =
        [
          "    %localenv_extract_array = bitcast %value* %env to " ^ array_type ^ "*",
          "    %localenv_extract = load " ^ array_type ^ "* %localenv_extract_array"
        ]
      val extracts = 
        List.foldr (fn (x,y) => 
          (case x 
            of (name,reg) => 
              ("    " ^ reg ^ " = extractvalue " 
              ^ array_type ^ " %localenv_extract, " 
              ^ itos (List.length y)) :: y
          )
        ) [] sym_env
    in
      bitcasts@extracts
    end

(*Converts output expression to curried functions*)
and make_curry [] expr = expr
  | make_curry (x::xs) expr = (I.EFun (x, (make_curry xs expr)))

end