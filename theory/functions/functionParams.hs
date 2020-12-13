data Tree a = Empty | Branch a (Tree a) (Tree a)

foldTree :: (e -> a -> a -> a) -> a -> Tree e -> a
foldTree f z Empty = z
foldTree f z (Branch x l r) = f x (foldTree f z l) (foldTree f z r)


squarelist :: [Int] -> [Int]
squarelist = map (\x -> x*x)
