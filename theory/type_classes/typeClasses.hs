-- traffic light
data Light = Red | Orange | Green

instance Eq Light where
    Red == Red = True
    Green == Green = True
    Orange == Orange = True
    _ == _ = False

-- evaluate expressions
data Exp = Lit Int | Add Exp Exp

eval :: Exp -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

instance Eq Exp where
    e1 == e2 = eval e1 == eval e2
    e1 /= e2 = not (e1 == e2)

-- constrained polymorphism
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- propagation of type constraints
nbDistinct :: Eq a => [a] -> Int
nbDistinct = length . nub

-- polymorphic instances
instance Show Light where
    show Green = "green light, go ahead"
    show Orange = "orange light, please make it to a stop"
    show Red = "red light, do not pass"

-- subclasses
instance Ord Light where
    compare Green Green = EQ
    compare Red Red = EQ
    compare Orange Orange = EQ
    compare Green _ = LT
    compare _ Green = GT
    compare Red _ = GT
    compare _ Red = LT

-- automatically derived instances
data Light' = Green' | Red' | Orange' deriving (Eq, Ord, Show)