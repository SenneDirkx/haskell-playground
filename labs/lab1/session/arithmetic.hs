-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush x) s = x:s
execute (IAdd) (x:y:s) = (x+y:s)
execute (ISub) (x:y:s) = (y-x:s)
execute (IMul) (x:y:s) = (x*y:s)
execute _ _ = runtimeError

run :: Prog -> Stack -> Stack
run [] s = s
run (i:is) s = run is (execute i s)

compile :: Exp -> Prog
compile (Const x) = [IPush x]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [IAdd]
compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [ISub]
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [IMul]
