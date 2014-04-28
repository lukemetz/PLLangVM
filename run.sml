CM.make "llvm.cm";
Control.Print.printDepth := 100;
structure P = Parser;
structure C = CompilerLLVM;

exception ParseError of string
fun parseError msg = raise ParseError msg

(*Read Lines from File*)
fun read_lines (infile : string) = 
  let
    val ins = TextIO.openIn infile
    fun loop ins =
      case TextIO.inputLine ins 
        of SOME line => line :: loop ins
         | NONE      => []
  in
    loop ins before TextIO.closeIn ins
  end

(*Grab file name from command line*)
val base_file = 
  let
    val file = hd (CommandLine.arguments())
  	val string_end = (String.size file) - 4
  in
  	String.substring (file, 0, string_end)
end

(*Write string to file*)
fun write fileName s = 
  let
  	val file = TextIO.openOut fileName
  	val _ = TextIO.output(file, s)
  	val _ = TextIO.closeOut file
  in
  	print "Written to llvm file. \n"
  end

(*Read file*)
fun read_file filename = List.foldr (fn (x,y) => x ^ y) "" (read_lines filename)

(*Runs the compiler*)
fun compile (P.DDef (str, ss, expr)) sym_env = C.compileDecl str ss expr sym_env
  | compile _ _ = parseError "Plang Compiler could not understand the file input"

(*Parses and feeds token list to compiler*)
fun generate_LLVM ts sym_env = 
  (case (P.parse_decl ts) 
    of SOME (decl, []) =>
      (case (compile decl sym_env) 
        of (body, sym_env) =>
          (body ^ "\n", sym_env)
      )
    |  SOME (decl, ts) => 
      (case (compile decl sym_env) 
        of (body, sym_env) =>
          (case generate_LLVM ts sym_env 
            of (body2,syn_env) => 
            ((body ^ "\n" ^  body2), sym_env)
          )
      )
    |  NONE => ("", sym_env)
  )

(*Runs the script*)
fun runPlang boiler initEnv =
  let 
    val flat_boiler = read_file boiler
    val flat_plg = read_file (base_file ^ ".plg")
    val result = 
      write ("build/" ^ base_file ^ ".ll") 
        ((case generate_LLVM (P.lexString flat_plg) initEnv 
            of (body, initEnv) => body
          )^ "\n" ^ flat_boiler)
  in
    print "All good in sml bro\n"
  end

val _ = runPlang "src/boiler.ll" [("print", "@print")]
val _ = OS.Process.exit(OS.Process.success)

