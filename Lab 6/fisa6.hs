minElement :: (Ord a) => [a] -> Maybe a
minElement [] = Nothing
minElement xs = Just (head (quicksort xs))

quicksort [] = []
quicksort (x : xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

fibs :: [Integer]
fibs = map fst (iterate (\(a, b) -> (b, a + b)) (0, 1))

fib n = fibs !! n

isPrime :: Integer -> Bool
isPrime n = if n > 1 then null [ x | x <- [2..k - 1], mod k x == 0] else False 

primeFlags :: [Bool]
primeFlags = map isPrime [0 ..]

--- fibs :: [Integer]
--- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)