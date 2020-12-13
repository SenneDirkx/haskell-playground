echo2 :: IO ()
echo2 = do  c1 <- getChar
            c2 <- getChar
            putChar '\n'
            putChar c1
            putChar c2
            putChar '\n'

replicateM :: Int -> IO a -> IO [a]
replicateM n m = sequence (replicate n m)

readNumbers :: Int -> IO [Int]
readNumbers n = replicateM n readLn