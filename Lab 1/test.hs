sum1 :: Int -> Int 
doubleSmallNumber :: Int -> Int 
sumThree :: Int -> Int -> Int -> Int 
idp :: Int -> Int
myMax :: Int -> Int -> Int 
myMaxOfThree :: Int -> Int -> Int -> Int 
mySum :: Int -> Int 
fib :: Int -> Int 
greatCommonDivisor :: Int -> Int -> Int 

doubleSmallNumber x = (if x > 100 then x else x*2) + 1
sumThree a b c = a + b + c
idp x = x
myMax x y = (if x <= y then y else x)
myMaxOfThree d e f = (if d > e && d > f then d else (if e > d && e > f then e else f))
mySum x = if x <= 0 then 0 else x + mySum (x - 1)
fib elementFib = if elementFib <= 1 then elementFib else fib (elementFib - 1) + fib (elementFib - 2)
greatCommonDivisor a b = if b == 0 then a else greatCommonDivisor b (mod a b)

sum1 0 = 0
sum1 n = n + sum1 (n - 1)