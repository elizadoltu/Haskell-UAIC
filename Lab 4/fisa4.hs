f :: Int -> Int -> Int 
f x y = x + y 

g :: String -> String -> String 
g x y = x ++ y 

h :: String -> String -> String 
h x y = y ++ x

-- Lambda
-- (\ x -> x + 3) 5
f' = \ x -> x + 3
f'' = \ x y -> x + y

flip' :: (a -> a -> a) -> (a -> a -> a)
flip' f = \ x y -> f y x
h' = flip' g

xyz :: (Int -> Int) -> [Int] -> [Int]
zyx f [] = []
xyz f (hd : tl) = (f hd) : (xyz f tl)

-- map (\ x -> x ++ "!") ["Salut", "Hello"]

reduce :: (Int -> Int -> Int) -> Int -> [Int] -> Int 
reduce f a [] = 0
reduce f a (hd : tl) = f hd (reduce f a tl)

-- foldl (++) "xyz" ["aa", "bb", "c"]
-- functia $
-- functie dolar argument - aplica functia pe argumentul respectiv

-- functia . 
-- ((.) odd length) "aaab"
-- (*3) . (+4) $ 10