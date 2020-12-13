-- * Haskell 101
-- ----------------------------------------------------------------------------

double :: Int -> Int
double n = n*2

myAbs :: Int -> Int
myAbs n
    | n < 0 = n*(-1)
    | otherwise = n

toFahrenheit :: Float -> Float
toFahrenheit c = 1.8*c + 32

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "fizzbuzz"
    | n `mod` 3 == 0 = "fizz"
    | n `mod` 5 == 0 = "buzz"
    | otherwise = show n

