CM.make "llvm.cm";
structure P = Parser;
structure C = CompilerLLVM 

exception Error of string

fun compile (P.DExpr expr) = C.compileExpr expr
  | compile _ = raise Error ("Invalid IR enclosing")
fun toLLVM str = compile (P.parseDecl str)

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

val test = readlist file
val flat_file = List.foldr (fn (x,y) => x ^ y) "" test

fun write fileName s = let
	val file = TextIO.openOut fileName
	val _ = TextIO.output(file, s)
	val _ = TextIO.closeOut file
in
	"All good bro"
end

val result = write (base_file ^ ".ll") (toLLVM flat_file);

val _ = OS.Process.exit(OS.Process.success)
