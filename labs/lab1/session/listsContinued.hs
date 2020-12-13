-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n
    | n < 0 = 1
    | otherwise = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n < 0 = []
    | otherwise = [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten list = [x | xs <- list, x <- xs]

range :: Int -> Int -> [Int]
range low high
    | low > high = []
    | otherwise = [low..high]

sumInts :: Int -> Int -> Int
sumInts a b = sum ( range a b )

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n xs = [y |y <- xs, y `mod` n /= 0]