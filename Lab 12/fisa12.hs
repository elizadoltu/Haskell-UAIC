type Id = String

data Term
  = Var Id
  | App Term Term
  | Lambda Id Term
  deriving (Show, Eq)

subst :: Id -> Term -> Term -> Term
subst id term (Var id')
  | id == id' = term
  | otherwise = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term')
  | id == id' = Lambda id' term'
  | otherwise = Lambda id' (subst id term term')

x = Var "x"

y = Var "y"

z = Var "z"

term1 = App x y

term2 = App y x

termLambda = Lambda "x" y 
termY = Var "y"

term3 = Lambda "x" term2

term4 = Lambda "y" term1

test1 = subst "x" y x -- x[x/y] = y

test2 = subst "y" z x -- x[y/z] = x

test3 = subst "y" z term1 -- (x y)[y/z] = x z

test4 = subst "y" z term2 -- (y x)[y/z] = z x

test5 = subst "x" (Lambda "z" z) term3 -- (λx.(y x))[x/(λz.z)] = λx.(y x)

test6 = subst "x" (Lambda "z" z) (Lambda "y" (App y x)) -- (λy.(y x))[x/(λz.z)] = λy.(y (λz.z))

tests = [test1, test2, test3, test4, test5, test6]

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd : tl)
  | id == hd = remove id tl
  | otherwise = hd : remove id tl

testRemove1 = remove "x" ["x", "y", "x", "z"] 

testRemove2 = remove "a" ["x", "y", "x", "z"] 

testRemove3 = remove "x" []

testRemove4 = remove "y" ["y", "y", "y"] 


free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

testFree1 = free term4
testFree2 = free term3

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term

testVars1 = vars term3 
testVars2 = vars term4

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

fresh' :: [Id] -> Int -> Id
fresh' ids index =
    if ("n" ++ show index) `elem` ids
        then fresh' ids (index + 1)
        else "n" ++ show index

testFresh1 = fresh' ["n0", "n1", "n2"] 0 

testFresh2 = fresh' ["n0", "n1", "n3"] 0 

testFresh3 = fresh' ["n0", "n1", "n2", "n3", "n4"] 0

testFresh4 = fresh' [] 0 

testFresh5 = fresh' ["n1", "n2", "n3"] 0 

testFresh6 :: Id
testFresh6 = fresh' ["n0", "n1", "n2"] 3

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _
  | id == id' = term
  | otherwise = Var id'
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid
  | id == id' = Lambda id' term'
  | id' `elem` (free term) =
      let id'' = fresh avoid
       in Lambda id'' (casubst id term (subst id' (Var id'') term') (id'' : avoid))
  | otherwise = Lambda id' (casubst id term term' avoid)

testCasubst1 = casubst "x" y x []

testCasubst2 = casubst "y" z x [] 

testCasubst3 = casubst "y" z term1 [] 

testCasubst4 = casubst "y" z term2 [] 

testCasubst5 = casubst "x" (Lambda "z" z) term3 [] 

testCasubst6 = casubst "x" (Lambda "z" z) (Lambda "y" (App y x)) [] 

testCasubst7 = casubst "y" x termLambda []


reduce1' :: Term -> [Id] -> Maybe Term 
reduce1' (Var id') _ = Nothing 
reduce1' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of 
    Nothing -> case reduce1' term2 avoid of 
        Nothing -> Nothing
        Just term2' -> Just (App term1 term2')
    Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of 
    Nothing -> Nothing 
    Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term 
reduce1 t = reduce1' t (vars t)

x' = Var "x"

y' = Var "y"

z' = Var "z"

term10 = Lambda "x" x
term12 = App term10 term10 
term13 = Lambda "y" (Lambda "x" term12)
term14 = App term13 term10 

ex1 = reduce1 term10
ex2 = reduce1 term12 
ex3 = reduce1 term13 
ex4 = reduce1 term14 

reduce :: Term -> Term 
reduce term = case reduce1 term of 
    Nothing -> term 
    Just term' -> reduce term'

infiniteLoopTerm :: Term
infiniteLoopTerm = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

tTRUE = Lambda "x" (Lambda "y" x')
tFALSE = Lambda "x" (Lambda "y" y')
