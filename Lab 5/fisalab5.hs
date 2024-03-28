--- Exercitiul 0.4
data Nat = Zero | Double Nat | DoubleAddOne Nat 

convert :: Nat -> Int 
convert Zero = 0
convert (Double n) =  2 * convert n
convert (DoubleAddOne n) = (2 * convert n) + 1

instance Show Nat where 
    show n = show (convert n) 

instance Eq Nat where 
    (==) Zero Zero = True 
    (==) Zero (Double Zero) = True 
    (==) Zero (Double (DoubleAddOne x)) = False
    (==) Zero (DoubleAddOne Zero) = False 
    (==) Zero (DoubleAddOne x) = False
    (==) (Double Zero) (Double Zero) = True 
    (==) (DoubleAddOne Zero) (DoubleAddOne Zero) = True 
    (==) (Double x) (Double y) = x == y
    (==) (DoubleAddOne x) (DoubleAddOne y) = x == y 
    (==) _ _ = False 

instance Ord Nat where 
    compare Zero Zero = EQ 
    compare Zero (Double Zero) = EQ  
    compare Zero (DoubleAddOne Zero) = LT 
    compare Zero (Double _) = LT  
    compare Zero (DoubleAddOne _) = LT
    compare (Double Zero) Zero = EQ 
    compare (DoubleAddOne Zero) Zero = GT  
    compare (Double _) Zero  = EQ 
    compare (DoubleAddOne _) Zero = GT  
    compare (Double x) (Double y) = compare x y 
    compare (DoubleAddOne x) (DoubleAddOne y) = compare x y 

--- Exercitiul 0.6
class MyOrd a where 
    lessThan :: a -> a -> Bool 
    equalTo :: a -> a -> Bool 
    greaterThan :: a -> a -> Bool

instance MyOrd Int where 
    lessThan x y = x < y
    greaterThan x y = x > y 
    equalTo x y = x == y  

instance (MyOrd a) => MyOrd [a] where 
    lessThan :: MyOrd a => [a] -> [a] -> Bool
    lessThan [] [] = False 
    lessThan [] _ = True 
    lessThan _ [] = False 
    equalTo [] [] = True 
    equalTo [] _ = False 
    equalTo _ [] = False 
    greaterThan [] [] = False 
    greaterThan [] _ = False
    greaterThan _ [] = True 


sortCustom :: (MyOrd a) => [a] -> [a]
sortCustom [] = []
sortCustom (hd : tl) = (sortCustom (filter (lessThan hd) tl)) ++ [hd] ++ (sortCustom (filter (greaterThan hd) tl))

--- Exercitiul 0.8 
data Natural = MyZero | Succ Natural

--- Exercitiul 0.16
instance Num Natural where 
    (+) MyZero MyZero = 0 
    (+) MyZero (Succ x) = Succ x 
    (+) (Succ x) MyZero = Succ x 
    (+) (Succ x) (Succ y) = Succ (x + y)  
    (*) MyZero _ = MyZero  
    (*) (Succ x) MyZero = MyZero 
    (*) (Succ x) (Succ y) = Succ(x * y) 
    abs MyZero = MyZero 
    abs (Succ x) = Succ x
    signum MyZero = MyZero 
    signum (Succ _) = Succ MyZero
    fromInteger x
        | x <= 0 = MyZero
        | otherwise = Succ (fromInteger (x - 1))
    (-) MyZero _ = MyZero 
    (-) x MyZero = x 
    (-) (Succ x) (Succ y) = x - y

instance Show Natural where 
    show MyZero = "o"
    show (Succ x) = "s" ++ show x

--- Exercitiul 0.9
instance Ord Natural where 
    compare MyZero MyZero = EQ 
    compare MyZero (Succ _) = LT 
    compare (Succ _) MyZero = GT 
    compare (Succ x) (Succ y) = compare x y 

--- Exercitiul 0.12 
instance Eq Natural where 
    (==) MyZero MyZero = True 
    (==) MyZero (Succ _) = False 
    (==) (Succ _) MyZero = False 
    (==) (Succ x) (Succ y) = x == y

--- Exercitiul 0.17 
data List a = Nil | Cons a (List a)

instance (Eq a) => Eq (List a) where 
    (==) Nil Nil = True
    (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
    (==) _ _ = False 

instance Functor List where
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)






