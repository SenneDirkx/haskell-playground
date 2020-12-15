import Data.Char
import Data.List
-- * Caesar Cipher
-- ----------------------------------------------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ( mod (let2int c + n) 26)
    | otherwise = c

encode :: Int -> String -> String
encode n str = [ shift n c | c <- str]

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent a b = 100.0*(fromIntegral a)/(fromIntegral b)

freqs :: String -> [Float]
freqs xs = [ percent (count c) n | c <- ['a'..'z']]
    where
        n = length (filter isLower xs)
        count x = length (filter (==x) xs)
    

chisqr :: [Float] -> [Float] -> Float
chisqr o e = sum [ (oi-ei)^2 / ei | (oi,ei) <- zip o e]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        table'      = freqs xs
        chitable    = [ chisqr (rotate n table') table | n <- [0..25] ]
        minchi      = minimum chitable
        Just factor = elemIndex minchi chitable
