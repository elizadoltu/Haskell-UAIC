data MobileDevice = Smartphone Culori
    | Laptop Culori
    | Tablet Culori
    deriving Show

data Culori = Red | Green 
    | Blue 
    | Orange 
    | Pink
    deriving Show

descriere :: MobileDevice -> Culori
descriere (Laptop x) = x 
descriere (Tablet x) = x
descriere (Smartphone x) = x

data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

minBST :: Arb -> Integer 
minBST Frunza = 0
minBST (Nod x l r) = minBST l 

maxBST :: Arb -> Integer 
maxBST Frunza = 0
maxBST (Nod x l r) = maxBST r

isBST :: Arb -> Bool 
isBST Frunza = True
isBST (Nod x l r) = ((maxBST l < x) && (minBST r > x) && isBST l && isBST r)

search :: Arb -> Integer -> Bool 
search Frunza valoare = valoare
search (Nod x l r) valoare  
    | x == valoare = True
    | x < valoare = search r valoare
    | otherwise = search l valoare

insert :: Arb -> Integer -> Arb
insert Frunza valoare = Nod (valoare Frunza Frunza)
insert (Nod x l r) valoare 
    | x < valoare = insert r valoare
    | x > valoare = insert l valoare
