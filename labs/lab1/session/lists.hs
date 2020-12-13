-- * List Operations
-- ----------------------------------------------------------------------------

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
    | n > x = x : insert n xs
    | n == x = x : n : xs
    | n < x = n : x : xs

myLast :: [Int] -> Int
myLast [] = error "Empty!"
myLast (x:xs)
    | xs == [] = x
    | otherwise = myLast xs

