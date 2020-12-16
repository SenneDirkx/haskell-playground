-------------------------------------------------------------------------------
-- PART I: Expense & Delta

-- IMPORTANT: MAKE SURE THE AMOUNT COMES BEFORE THE NAME
data Expense = MkExpense Double String deriving (Eq,Ord)

mkExpense :: String -> Double -> Expense
mkExpense s d = MkExpense d s

instance Show Expense where
  show (MkExpense d s) = s ++ ": " ++ show d

data Delta = MkDelta Expense deriving (Eq,Ord)

instance Show Delta where
  show (MkDelta e) = show e

fromExpense :: Double -> Expense -> Delta
fromExpense avg (MkExpense d s) = MkDelta (MkExpense (d-avg) s)

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

-- | Convert a list of Expenses to a list of Deltas
-- The deltas are with respect to the average expense.
toDeltas :: [Expense] -> [Delta]
toDeltas es = calcDeltas avg es
    where
        f (MkExpense d s) rest = d + rest
        g e rest = 1 + rest
        sumExpenses = foldr f 0.0
        countExpenses = foldr g 0
        total = sumExpenses es
        amount = countExpenses es
        avg = total/amount
        calcDeltas a [] = []
        calcDeltas a ((MkExpense d s):xs) = (MkDelta (MkExpense (d-a) s)):(calcDeltas a xs)

-------------------------------------------------------------------------------
-- PART II: Transferable Transfers

-- | The Transfer datatype: a money transfer from one person to another.
data Transfer = MkTransfer String String Double deriving Eq

trans_from :: Transfer -> String
trans_from (MkTransfer from _ _) = from

trans_to :: Transfer -> String
trans_to (MkTransfer _ to _) = to

trans_amount :: Transfer -> Double
trans_amount (MkTransfer _ _ amount) = amount

instance Show Transfer where
  show (MkTransfer from to amount) = from ++ " -> " ++ to ++ ":" ++ show amount

-- | The Transferable class contains types t to which a transfer can be applied,
-- and that can create a Transfer from one t to another t
class Transferable t where
  applyTransfer  :: Transfer -> t -> t

-- | Apply a list of Transfers to to a Transferable from left to right.
applyTransfers :: Transferable t => [Transfer] -> t -> t
applyTransfers transfers x = foldl (flip applyTransfer) x transfers

instance Transferable Expense where
    applyTransfer (MkTransfer payer payee amount) (MkExpense d owner)
        | owner == payee && owner == payer = MkExpense d owner
        | owner == payer = MkExpense (d+amount) owner
        | owner == payee = MkExpense (d-amount) owner
        | otherwise = MkExpense d owner

instance Transferable Delta where
    applyTransfer (MkTransfer payer payee amount) (MkDelta (MkExpense d owner))
        | owner == payee && owner == payer = MkDelta (MkExpense d owner)
        | owner == payer = MkDelta (MkExpense (d+amount) owner)
        | owner == payee = MkDelta (MkExpense (d-amount) owner)
        | otherwise = MkDelta (MkExpense d owner)

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer amount (MkDelta (MkExpense d1 s1)) (MkDelta (MkExpense d2 s2))
    = MkTransfer s1 s2 amount

-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced expenses epsilon = maximum amounts - minimum amounts < epsilon
    where
        amounts = map (\(MkExpense d s) -> d) expenses

deltaAmount :: Delta -> Double
deltaAmount (MkDelta (MkExpense d s)) = d

-- | Epsilon-balance a list of Deltas.
balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas deltas epsilon =
    let payee = maximum deltas
        payer = minimum deltas
        amount = min (abs (deltaAmount payee)) (abs (deltaAmount payer))
        transfer = createTransfer amount payer payee
    in  if deltaAmount payee - deltaAmount payer < epsilon then
            []
        else
            transfer : balanceDeltas (map (applyTransfer transfer) deltas) epsilon

-- | Epsilon-balance a list of Expenses.
balance :: [Expense] -> Double -> [Transfer]
balance expenses = balanceDeltas (toDeltas expenses)


-------------------------------------------------------------------------------
-- PART IV: Application

getExpense :: IO (Maybe Expense)
getExpense = do
    name <- putStr "Name: " >> getLine
    amount <- putStr "Amount: " >> readLn
    if amount >= 0 then
        return (Just (MkExpense amount name))
    else
        return Nothing

-- | Read a list of expenses.
-- If the entered amount is non-negative, the list is terminated.
getExpenses :: IO [Expense]
getExpenses = do
    me <- getExpense
    case me of
        Nothing -> return []
        Just e -> fmap (e:) getExpenses

-- | Print a list of transfers, each on a separate line 
printTransfers :: [Transfer] -> IO ()
printTransfers = mapM_ print

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = do
    expenses <- getExpenses
    putStrLn ""
    printTransfers (balance expenses 0.01)