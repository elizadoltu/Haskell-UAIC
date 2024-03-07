and_1 :: Bool -> Bool -> Bool 
and_1 False _ = False 
and_1 _ False = False 
and_1 _ _ = True 

or_1 :: Bool -> Bool -> Bool 
or_1 False _ = True 
or_1 _ False = True 
or_1 _ _ = False 

not_1 :: Bool -> Bool 
not_1 True = False 
not_1 False = True 

nand_1 :: Bool -> Bool -> Bool 
nand_1 False _ = True
nand_1 _ False = True 
nand_1 _ _ = True 

nor_1 :: Bool -> Bool -> Bool 
nor_1 False _ = False 
nor_1 _ False = False 
nor_1 _ _ = True 

isPrime :: Integer -> Bool 
--isPrime :: Integer -> Bool 
hasDivisors :: Integer -> Integer -> Integer -> Bool 
fibo :: Integer -> Integer
euclidImpartiri :: (Integer , Integer) -> Integer
euclidScaderi :: (Integer, Integer) -> Integer
fiboaux :: Integer -> Integer -> Integer -> Integer
fibo' :: Integer -> Integer 
succesor :: Integer -> Integer 
adunareSuccesiva :: Integer -> Integer -> Integer
inmultire :: Integer -> Integer -> Integer
ridicarePutere :: Integer -> Integer -> Integer
--euclidExtins :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
modImpl :: Integer -> Integer -> Integer
divImpl :: Integer -> Integer -> Integer 

modImpl a b 
    | b == 0 = -1
    | otherwise = mod a b 

divImpl a b 
    | b == 0 = -1
    | otherwise = div a b 

succesor n = n + 1

adunareSuccesiva a b 
    | b == 0 = a 
    | otherwise = adunareSuccesiva (succesor a) (b - 1)

inmultire a b
    | b == 0 = a 
    | otherwise = inmultire (adunareSuccesiva a b) (b - 1)

ridicarePutere baza exponent 
    | exponent == 0 = 1 
    | otherwise = ridicarePutere (inmultire baza exponent) (exponent - 1)

fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n - 1) (a + b) a

fibo' n = fiboaux n 0 1

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

euclidImpartiri (a, b)
    | a == 0 = b 
    | otherwise = euclidImpartiri (mod b a, a)
  
euclidScaderi (a, b) 
    | a > b = euclidScaderi (a - b, b)
    | b > a = euclidScaderi (a, b - a)
    | otherwise = a

