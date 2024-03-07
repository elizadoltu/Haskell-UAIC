and :: Bool -> Bool -> Bool 
and False _ = False 
and _ False = False 
and _ _ = True 

or :: Bool -> Bool -> Bool 
or False _ = True 
or _ False = True 
or _ _ = False 

not :: Bool -> Bool 
not True = False 
not False = True 

nand :: Bool -> Bool -> Bool 
nand False _ = True
nand _ False = True 
nand _ _ = True 

nor :: Bool -> Bool -> Bool 
nor False _ = False 
nor _ False = False 
nor _ _ = True 

isPrime :: Integer -> Bool 
--isPrime :: Integer -> Bool 
hasDivisors :: Integer -> Integer -> Integer -> Bool 
fibo :: Integer -> Integer

fibo element 
    | element <= 1 = element 
    | otherwise = fibo (element - 1) + fibo (element - 2)

hasDivisors n a b
  | a > b = False
  | mod n a == 0 = True 
  | otherwise = hasDivisors n (a + 1) b

isPrime n 
    | hasDivisors n 2 (n - 1) = False  
    | otherwise = True