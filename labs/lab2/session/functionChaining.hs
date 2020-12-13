-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll xs a = foldr ($) a xs

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = applyAll (replicate n f)

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x = map ($x)
