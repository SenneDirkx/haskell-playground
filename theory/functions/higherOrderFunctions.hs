-- anonymous functions
squarelist :: [Int] -> [Int]
squarelist l = map square l

square :: Int -> Int
square x = x*x

-- currying
add :: Int -> (Int -> Int)
add x = \y -> y + x

inc :: Int -> Int
inc = add 1

-- partial application
add' :: Int -> Int -> Int
add' x y = y + x

inc' :: Int -> Int
inc' = add' 1

-- operator sections
inc'' :: Int -> Int
inc'' = (+) 1

positive :: Int -> Bool
positive = (>0)

-- eta reduction
sum' :: [Int] -> Int
sum' l = foldr (+) 0 l

sum'' :: [Int] -> Int
sum'' = foldr (+) 0

-- function composition
wc :: String -> Int
wc str = length (words str)

wc' :: String -> Int
wc' = length . words

-- pipelines
squareSumEvens :: [Int] -> Int
squareSumEvens = sum . map (\x -> x*x) . filter even

-- point-wise pipelines
squareSumEvens' :: [Int] -> Int
squareSumEvens' l = sum $ map (\x -> x*x) $ filter even l


