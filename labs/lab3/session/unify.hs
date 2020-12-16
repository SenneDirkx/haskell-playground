import Data.List (intercalate)

type VarId = Int

-- * Prolog terms
-- ----------------------------------------------------------------------------

data Term            -- Corresponding term:
  = F String [Term]  -- f(t1,...,tn)
  | Var VarId        -- Xn
  deriving (Eq,Ord)

instance Show Term where
  show (F name []) = name
  show (F name terms) = name ++ "(" ++ intercalate "," (map show terms) ++ ")"
  show (Var n) = "X" ++ show n

-- | Occurs check.
--
-- @occurs n t@ returns 'True' if and only if @Var n@ occurs in @t@
occurs :: VarId -> Term -> Bool
occurs n (Var x) = n == x
occurs n (F _ terms) = any (occurs n) terms

-- * Substitutions
-- ----------------------------------------------------------------------------

type Substitution = [(VarId,Term)]

-- | Apply as substitution to a Term
applySubst :: Substitution -> Term -> Term
applySubst subst (Var x)
    | Just t <- lookup x subst = t
    | otherwise = Var x
applySubst subst (F f args) = F f (map (applySubst subst) args)

-- | Concatenate two substitutions:
-- Apply the first substitution to all the terms in the second substitution,
-- then concatenate the lists.
conc :: Substitution -> Substitution -> Substitution
conc s1 s2 = s1 ++ [(n, applySubst s1 t) | (n,t) <- s2]

-- | Unify two terms
unify1 :: Term -> Term -> Maybe Substitution
unify1 t1 t2 = unify [(t1,t2)]

-- | Solve a set of equations.
--
-- @unify eqs@ returns @Just t@ where @t@ is the most general unifier of
-- @eqs@ if it exists, otherwise it returns @Nothing@
unify :: [(Term,Term)] -> Maybe Substitution
unify [] = Just []
unify ((Var n, Var m):eqs)
    | m == n = unify eqs
unify ((Var n, t):eqs) =
    let app = applySubst [(n,t)]
    in fmap (`conc` [(n,t)]) (unify [(app t1, app t2) | (t1,t2) <- eqs])
unify ((t, Var n):eqs) = unify ((Var n, t):eqs)
unify ((F f1 args1, F f2 args2):eqs)
    | f1 == f2 && length args1 == length args2 = unify (zip args1 args2 ++ eqs)
unify _ = Nothing