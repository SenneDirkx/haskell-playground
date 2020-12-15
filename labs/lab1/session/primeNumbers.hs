import Data.List ((\\))

-- * Exercise 5: Prime Numbers
-- ----------------------------------------------------------------------------

sieve :: Int -> [Int]
sieve m = f [2..(m-1)]
  where
    f (x:xs) = x : f (xs \\ [x,x+x..m])
    f []     = []

-- -------------------------
-- Some useful functions
-- -------------------------
sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor
-- -------------------------

floorSquare :: Int -> Int
floorSquare n = floorMono (sqrtMono (i2d n))

fastSieve :: Int -> [Int]
fastSieve m = f [2..(m-1)]
  where
    f (x:xs)
        | x <= s = x : f (xs \\ [x,x+x..m])
        | otherwise = x:xs
    f []     = []
    s = floorSquare m

