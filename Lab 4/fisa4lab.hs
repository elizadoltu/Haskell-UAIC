-- addThree :: (Int, Int, Int) -> Int
-- addThree (x, y, z) = x + y + z

-- varianta curry ex 1.1
addThree :: Int -> Int -> Int -> Int 
addThree x y z = x + y + z

-- exercitii 2
process :: (a -> a) -> a -> a
process f x = f x

-- exercitiul 2.1 
function_ex2 :: (Int -> Int) -> Int -> Int -> Int 
function_ex2 f x y
  | x == y = f x
  | x < y = f x + function_ex2 f (x + 1) y

-- exercitiul 2.2
func :: (b -> c) -> (a -> b) -> a -> c
func f g x = f (g x)

-- exercitiul 2.4 
sumList :: [Int] -> Int
sumList [] = 0 
sumList (hd : tl) = hd + (sumList tl)
-- sumList tl = foldr (+) 0 tl

-- exercitiul 2.5 
sumListFunc :: (Int -> Int) -> [Int] -> [Int] 
sumListFunc f [] = []
sumListFunc f (hd : tl) = f hd : sumListFunc f tl
-- sumListFunc f tl = map f tl

-- exercitiul 2.6 
funcBool :: (a -> Bool) -> [a] -> [a]
funcBool p [] = []
funcBool p (hd : tl) = if p hd then 
                            hd : funcBool p tl
                        else 
                            funcBool p tl

-- exercitiul 2.7 
-- foldl (++) "xyz" ["aa", "bb", "c"]

-- exercitiul 3.1 
funcCompare :: (Ord a) => a -> a -> Bool
funcCompare x y
  | x <= y = True
  | otherwise = False

quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort _ [] = []
quicksort f (x : xs) =
  let smallerSorted = quicksort f [a | a <- xs, f a x]
      biggerSorted = quicksort f [a | a <- xs, not (f a x)]
   in smallerSorted ++ [x] ++ biggerSorted

-- exercitiul 3.2 
