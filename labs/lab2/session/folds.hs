-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base [] = base
foldInts fn base (x:xs) = fn x (foldInts fn base xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _fn base [] = base
myFoldl fn base (x:xs) = myFoldl fn (fn base x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _fn base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)

readInBase :: Int -> [Int] -> Int
readInBase base digits = myFoldl (\x xs -> x*base + xs) 0 digits

myMap :: (a -> b) -> [a] -> [b]
myMap _fn [] = []
myMap fn (x:xs) = (fn x : myMap fn xs)

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn list = foldr (\x xs -> fn x : xs) [] list

