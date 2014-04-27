CM.make "llvm.cm";
Control.Print.printDepth := 100;
structure P = Parser;
structure C = CompilerLLVM;

exception Error of string

fun compile (P.DDef (str, ss, expr)) sym_env = C.compileDecl str ss expr sym_env

fun toLLVM ts sym_env = (case (P.parse_decl ts) of
	SOME (decl, []) =>
      (case (compile decl sym_env) of 
        (body, sym_env) =>  (body ^ "\n", sym_env))
| SOME (decl, ts) => (case (compile decl sym_env) of 
  (body, sym_env) => (case toLLVM ts sym_env 
    of (body2,syn_env) => ((body ^ "\n" ^  body2), sym_env)))


| NONE => ("", sym_env)
)

fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end

val file = hd (CommandLine.arguments())

val base_file = let
	val string_size = String.size file
	val string_end = string_size - 4
in
	String.substring (file, 0, string_end)
end

val testCode = readlist file
val flat_file = List.foldr (fn (x,y) => x ^ y) "" testCode
val first_line = (P.parse_decl (P.lexString flat_file))

fun write fileName s = let
	val file = TextIO.openOut fileName
	val _ = TextIO.output(file, s)
	val _ = TextIO.closeOut file
in
	"All good bro"
end

fun read_file filename = List.foldr (fn (x,y) => x ^ y) "" (readlist filename)
val flat_boiler = read_file "src/boiler.ll"

val sym_env = [("print", "@print")]


val result = write ("build/" ^ base_file ^ ".ll") 
  ((case toLLVM (P.lexString flat_file) sym_env of (body, sym_env) => body) 
  ^ "\n" ^ flat_boiler)
val _ = print "All good in sml bro\n"
val _ = OS.Process.exit(OS.Process.success)

