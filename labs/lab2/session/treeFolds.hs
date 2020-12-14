data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f _ (Leaf x) = f x
foldTree f g (Fork t1 t2) = g (foldTree f g t1) (foldTree f g t2)

sumTree :: Tree Int -> Int
sumTree = foldTree (\x -> x) (+)

treeToList :: Tree a -> [a]
treeToList = foldTree (\x -> [x]) (++) 

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\x -> 1) (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\x -> 1) (\y z -> 1 + max y z)

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree (\x -> Leaf x) (\y z -> Fork z y)

minTree :: Tree Int -> Int
minTree = foldTree (\x -> x) min

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x -> Leaf (x+1)) (\y z -> Fork y z)

idTree :: Tree a -> Tree a
idTree = foldTree (\x -> Leaf x) (\y z -> Fork y z)
