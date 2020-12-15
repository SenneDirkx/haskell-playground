import Data.List (intersperse, intercalate)

-- 1.1
data LineItem = Space | NewLine | Word String
    deriving (Eq
             )

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = NewLine

mkWord :: String -> LineItem
mkWord = Word

-- 1.3
lineItemToStr :: LineItem -> String
lineItemToStr Space = "\" \""
lineItemToStr NewLine = "\"\\n\""
lineItemToStr (Word str) = "\"" ++ str ++ "\""

-- Comment out and complete this code
instance Show LineItem where
    show = lineItemToStr

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems [] = []
toLineItems (x:xs)
    | x == ' ' = Space : toLineItems xs
    | x == '\n' = NewLine : toLineItems xs
    | otherwise = word : toLineItems other
        where
            (before,after) = span (\y -> y /= ' ' && y /= '\n') (x:xs)
            word = Word before
            other = after

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems [] = ""
fromLineItems (x:xs) = removeQuotation (show x) ++ fromLineItems xs
    where
        removeQuotation [] = ""
        removeQuotation ('\\':'n':xs) = '\n' : removeQuotation xs
        removeQuotation (x:xs)
            | x == '\"' = removeQuotation xs
            | otherwise = x : removeQuotation xs


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces [] = []
removeSpaces (Space:xs) = removeSpaces xs
removeSpaces (x:xs) = x : removeSpaces xs

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = [[]]
splitInLines xs = before : after'
    where
        (before, after) = break (== NewLine) xs
        after'
            | _:xs' <- after = splitInLines xs'
            | otherwise = []

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords n ls
    | [] <- before = newTail
    | otherwise = before : newTail
        where
            wordCount (Word w) = length w
            wordCount _ = 0
            (before, after) = break (\w -> wordCount w > n) ls
            newTail
                | (tooLong:after') <- after = [tooLong] : separateTooLongWords n after'
                | otherwise = []
        

-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap lineWidth = go [] (lineWidth + 1)
  -- One extra space to offset the space of the first word
  where
    go acc _          [] = [reverse acc]
    go acc spaceLeft (Word w:is)
        | length w + 1 <= spaceLeft || length w > lineWidth
        = go (Word w:acc) (spaceLeft - length w - 1) is
        | otherwise
        = reverse acc : go [] (lineWidth + 1) (Word w:is)

-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces = intersperse Space

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines = intercalate [NewLine]

-- DO NOT CHANGE THIS FUNCTION
wordWrap :: Int -> String -> String
wordWrap lineWidth =
    fromLineItems .
    joinLinesWithNewlines .
    map joinLinesWithNewlines .
    map (map joinLineWithSpaces . concatMap (wrap lineWidth)) .
    map (separateTooLongWords lineWidth) .
    splitInLines .
    removeSpaces .
    toLineItems


-- 3.1
getLines :: IO String
getLines = do
    line <- getLine
    if line == "STOP"
        then return ""
        else ((line ++ "\n") ++) <$> getLines

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = do
    putStr "Please enter a line width: "
    lineWidth <- readLn
    putStrLn "Please enter a text to wrap:"
    text <- getLines
    putStrLn $ wordWrap lineWidth text