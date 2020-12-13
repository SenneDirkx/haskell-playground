-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat m
    | m == Rock = Paper
    | m == Paper = Scissors
    | m == Scissors = Rock

lose :: Move -> Move
lose m
    | m == Rock = Scissors
    | m == Paper = Rock
    | m == Scissors = Paper

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | beat m2 == m1 = Win
    | beat m1 == m2 = Lose
    | otherwise = Draw

