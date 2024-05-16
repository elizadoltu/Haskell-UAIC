import Prelude hiding (Left, Right)

data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

t1 :: Arb
t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

f :: Arb -> Int -> Arb
f (Node x (Node y (Node o a1 a2) a3) z) v = (Node x (Node y (Node v a1 a2) a3) z)

data Dir = L | R deriving (Show, Eq)

type Poz = [Dir]
type Zipper = (Arb, [Crumb])
type ListZipper a = ([a], [a])

changeList :: ListZipper a -> a -> Maybe (ListZipper a)
changeList ([], _) _ = Nothing
changeList (x : xs, bs) v = Just (v : xs, bs)

goLeftList :: ListZipper a -> Maybe (ListZipper a)
goLeftList ([], _) = Nothing
goLeftList (x : xs, bs) = Just (xs, x : bs)

goRightList :: ListZipper a -> Maybe (ListZipper a)
goRightList (_, []) = Nothing
goRightList (xs, b : bs) = Just (b : xs, bs)

-- goUpList :: ListZipper a -> Maybe (ListZipper a)
-- goUpList = goLeftList

at :: Arb -> Poz -> Arb
at a [] = a
at (Node _ a1 _) (L : p) = at a1 p
at (Node _ _ a2) (R : p) = at a2 p

change :: Arb -> Poz -> Int -> Arb
change Nil [] _ = error "Nu exista valoarea veche"
change Nil _ _ = error "Nu exista pozitia"
change (Node o a1 a2) [] v = Node v a1 a2
change (Node x a1 a2) (L : p) v = (Node x (change a1 p v) a2)
change (Node x a1 a2) (R : p) v = (Node x a1 (change a2 p v))

p = [L, L]

---data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)
data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)

type Trail = [Crumb]

-- goLeft :: (Arb, Trail) -> (Arb, Trail)
-- goLeft (Nil, _) = error "Cannot go left in leaf"
-- goLeft (Node x a1 a2, t) = (a1, (Left x a2) : t)

goLeft :: Zipper -> Maybe Zipper
goLeft (Node x l r, bs) = Just (l, Left x r : bs)
goLeft (Nil, _) = Nothing

-- goRight :: (Arb, Trail) -> (Arb, Trail)
-- goRight (Nil, _) = error "Cannot go left in leaf"
-- goRight (Node x a1 a2, t) = (a2, (Right x a1) : t)

goRight :: Zipper -> Maybe Zipper 
goRight (Node x l r, bs) = Just (l, Right x r : bs)
goRight (Nil, _) = Nothing


-- goUp :: (Arb, Trail) -> (Arb, Trail)
-- goUp (a, []) = error "Cannot go up in root"
-- goUp (a, ((Left x a2) : t)) = ((Node x a a2), t)
-- goUp (a, ((Right x a1) : t)) = ((Node x a1 a), t)

goUp :: Zipper -> Maybe Zipper 
goUp (t, Left x r : bs) = Just (Node x t r, bs)
goUp (t, Right x r : bs) = Just (Node x t r, bs)
goUp (_, []) = Nothing

change' :: Zipper -> Int -> Maybe Zipper
change' (Nil, _) _ = Nothing
change' (Node o a1 a2, bs) v = Just (Node v a1 a2, bs)

data Tree = Node' Int [Tree] deriving (Show, Eq)
data Direction = DownDir | RightDir deriving (Show, Eq)

type Crumb' = (Int, [Tree], [Tree])
type ZipperTree = (Tree, [Crumb'])

atPos :: [Tree] -> [Direction] -> Int
atPos [] _ = error "Empty"
atPos (Node' x children : _) [] = x 
atPos (Node' _ children : trees) (DownDir:ds) = atPos children ds 
atPos (_ : trees) (RightDir:ds) = atPos trees ds
 
t = Node' 10 [Node' 3 [],Node' 4 [Node' 1 [], Node' 2 [], Node' 22 [], Node' 33[] ],Node' 5 [], Node' 6 []]
-- at :: Arb -> Poz -> Arb
-- at a [] = a
-- at (Node _ a1 _) (L : p) = at a1 p
-- at (Node _ _ a2) (R : p) = at a2 p
