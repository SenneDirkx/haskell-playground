foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' = foldr' (\x xs -> 1 + xs) 0 

any' :: (Int -> Bool) -> [Int] -> Bool
any' f xs = foldr' (\y ys -> f y || ys) False xs

all' :: (Int -> Bool) -> [Int] -> Bool
all' f xs = foldr' (\y ys -> f y && ys) True xs

map' :: (Int -> Int) -> [Int] -> [Int]
map' f xs = foldr' (\y ys -> (f y):ys) [] xs

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' f xs = foldr' (\y ys -> if f y then y:ys else ys) [] xs

-- * Given helpers

even' :: Int -> Bool
even' = even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven = length' . filter' even'

onlyOdd :: [Int] -> [Int]
onlyOdd = filter' (not' . even')

absGtFive :: [Int] -> Int
absGtFive = length' . filter' (greaterThanFive . absolute')

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive xs = any' greaterThanFive (filter' even' xs)

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' xs = 0 < length' (filter' greaterThanFive (filter' even' xs))