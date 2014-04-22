CM.make "llvm.cm";
Control.Print.printDepth := 100;
structure P = Parser;
structure C = CompilerLLVM;

exception Error of string

fun compile (P.DExpr expr) = C.compileExpr expr
  | compile (P.DDef (str, ss, expr)) = C.compileDecl str ss expr

fun toLLVM ts = (case (P.parse_decl ts) of
	SOME (decl, []) => let
	  val header = ";Compiled by PLLangVM"
    val type_decl = "%value = type {i8, i32*}"
    val type_decl_env = "%environment = type {i32, %value, %environment * }"
                    in
    type_decl ^ "\n" ^ type_decl_env ^ "\n" ^ (compile decl) ^ "\n" ^ header ^"\n\n"
                    end
| SOME (decl, ts) => (compile decl) ^ "\n" ^  (toLLVM ts)
| NONE => ""
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
val result = write ("build/" ^ base_file ^ ".ll") (toLLVM (P.lexString flat_file) ^ "\n" ^ flat_boiler);

val _ = OS.Process.exit(OS.Process.success)
