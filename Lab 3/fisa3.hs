import Distribution.SPDX (LicenseId(VSL_1_0))
data Rezultat = Invalid | Valid Int deriving Show 

myDiv :: Int -> Int -> Rezultat
myDiv x 0 = Invalid 
myDiv x y = Valid (div x y)

aduna3 :: Rezultat -> Int 
aduna3 Invalid = 42
aduna3 (Valid x) = x + 3

data Lista = Nil | Cons Int Lista deriving Show 
count :: Lista -> Int 
count Nil = 0 
count (Cons hd tl) = 1 + count tl 

suma :: Lista -> Int 
suma Nil = 0 
suma (Cons hd tl) = hd + suma tl 

impare :: Lista -> Lista 
impare Nil = Nil 
impare (Cons hd tl) = if even hd then 
                        impare tl 
                      else Cons hd (impare tl)

incrlist :: Lista -> Lista 
incrlist Nil = Nil 
incrlist (Cons hd tl) = Cons  (hd + 1) (incrlist tl)

data GList a = GNil | GCons a (GList a) deriving Show 

gcount :: GList a -> Int
gcount GNil = 0 
gcount (GCons _ tl) = 1 + gcount tl 

ghead :: GList a -> Maybe a 
ghead GNil = Nothing 
ghead (GCons hd tl) = Just hd

ghead' :: [a] -> Maybe a 
ghead' [] = Nothing 
ghead' (hd : tl) = Just hd

convert :: [a] -> GList a 
convert [] = GNil 
convert (hd : tl) = GCons hd (convert tl);

data Pair = PairCons Int Int deriving Show 