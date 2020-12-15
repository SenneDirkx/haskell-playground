import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort l = smallest : selectionSort(delete smallest l)
    where
        smallest = minimum l

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p = foldr go ([], [])
    where
        go x (left, right)
            | p x = (x:left,right)
            | otherwise = (left,x:right)

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (filter p l, filter (not . p) l)

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = ([x | x <- l, p x], [x | x <- l, not (p x)])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort left ++ [p] ++ quicksort right
    where
        (left, right) = partitionFold (<= p) xs