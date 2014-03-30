CM.make "llvm.cm";
structure P = Parser;
structure C = CompilerLLVM 
fun compile (P.DExpr expr) = C.compileExpr expr
fun toLLVM str = compile (P.parseDecl str)
val t = toLLVM "(+ (+ 1 1) (+ 1 (+ 1 3)))"