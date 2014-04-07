structure Primitives = struct

  structure S = StackRepresentation


  exception Primitive of string
  fun primError s = raise Primitive (s)

  (* 
   *   Primitive operations
   *)

  val trueVal = S.VInt 1
  val falseVal = S.VInt 0

  fun primAdd ((S.VInt i)::(S.VInt j)::stack) = (S.VInt (i+j))::stack
    | primAdd _ = primError "primAdd"

  fun primMul ((S.VInt i)::(S.VInt j)::stack) = (S.VInt (i*j))::stack
    | primMul _ = primError "primMul"

  fun primSub ((S.VInt i)::(S.VInt j)::stack) = (S.VInt (i-j))::stack
    | primSub _ = primError "primMul"

  fun primMod ((S.VInt i)::(S.VInt j)::stack) = (S.VInt (i mod j))::stack
    | primMod _ = primError "primMul"

  fun primDup (v::stack) = v::v::stack
    | primDup _ = primError "primDup"

  fun primSwap (v::w::stack) = w::v::stack
    | primSwap _ = primError "primSwap"

  fun primOver (v::w::stack) = w::v::w::stack
    | primOver _ = primError "primOver"

  fun primPick ((S.VInt n)::stack) = let
        fun loop 0 (v::_) = v
	  | loop n (v::st) = loop (n-1) st
	  | loop _ _ = primError "primPick - empty stack"
      in
        if n<0 then
	  primError "primPick - negative argument"
	else (loop n stack)::stack
      end
    | primPick _ = primError "primOver"

  fun primRot (u::v::w::stack) = w::u::v::stack
    | primRot _ = primError "primRot"

  fun primDrop (v::stack) = stack
    | primDrop _ = primError "primDrop"

  fun primClosure ((S.VInt n)::(S.VCode s)::stack) = let
        fun loop 0 _ = []
	  | loop n (v::st) = v::(loop (n-1) st)
	  | loop _ _ = primError "primPack - empty stack"
      in
        (S.VClosure (s,Vector.fromList (loop n stack)))::stack
      end
    | primClosure _ = primError "primClosure"

  fun primCode ((S.VClosure (s,_))::stack) = (S.VCode s)::stack
    | primCode _ = primError "primCode"

  fun primRef ((S.VInt n)::(S.VClosure (_, v))::stack) = (Vector.sub (v,n))::stack
    | primRef _ = primError "primRef"

  fun primZeroEq ((S.VInt 0)::stack) = trueVal::stack
    | primZeroEq (_::stack) = falseVal::stack
    | primZeroEq _ = primError "primZeroEq"

  fun primEq (v1::v2::stack) = 
        (if equal v1 v2 then trueVal else falseVal)::stack
    | primEq _ = primError "primEq"

  and equal (S.VInt i) (S.VInt j) = i = j
    | equal (S.VList l1) (S.VList l2) = equalLists l1 l2
    | equal _ _ = false

  and equalLists [] [] = true
    | equalLists (v1::vs1) (v2::vs2) = 
      (if equal v1 v2 then equalLists vs1 vs2 else false)
    | equalLists _ _ = false

  fun primZeroGt ((S.VInt i)::stack) = if (i > 0) then trueVal::stack else falseVal::stack
    | primZeroGt (_::stack) = falseVal::stack
    | primZeroGt _ = primError "primZeroEq"

  fun primCons (v::(S.VList vs)::stack) = (S.VList (v::vs))::stack
    | primCons _ = primError "primCons"

  fun primHead ((S.VList (v::vs))::stack) = v::stack
    | primHead _ = primError "primHead"

  fun primTail ((S.VList (v::vs))::stack) = (S.VList vs)::stack
    | primTail _ = primError "primTail"

  fun primNilEq ((S.VList [])::stack) = trueVal::stack
    | primNilEq (_::stack) = falseVal::stack
    | primNilEq _ = primError "primNilEq"

  fun primShowStack stack = (print (" *** "^(S.stringOfStack stack 10)^"\n");
                             stack)

  fun primNil stack = (S.VList [])::stack

  fun primEmptyStack [] = [trueVal]
    | primEmptyStack stack = falseVal::stack



end
