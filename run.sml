CM.make "llvm.cm";
Control.Print.printDepth := 100;
structure P = Parser;
structure C = CompilerLLVM;

exception Error of string

fun compile (P.DExpr expr) = C.compileExpr expr
  | compile (P.DDef (str, ss, expr)) = C.compileDecl str ss expr

fun toLLVM ts = (case (P.parse_decl ts) of
	SOME (decl, ts) => (compile decl) ^ "\n" ^  (toLLVM ts)
	|_ => (let
	val header = ";Compiled by PLLangVM"
    val boiler = "@printFmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\ndefine i32 @print(i32 %a) {\n  entry:\n    %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @printFmt, i32 0, i32 0), i32 %a)\n    ret i32 0\n}\ndeclare i32 @printf(i8*, ...)\n"
in
    header ^ "\n\n" ^ boiler
end))


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

val result = write ("build/" ^ base_file ^ ".ll") (toLLVM (P.lexString flat_file));

val _ = OS.Process.exit(OS.Process.success)
