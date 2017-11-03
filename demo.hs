import Test.QuickCheck

fact2 0 = 1
fact2 n = n * fact2(n-1)
 
getstuff = do
             putStrLn "Enter stuff"
             x <- getLine
             putStrLn x
-- is this a comment? yes

newhead = head [1,2,3,4]

newtake = take 3 [5,4,3,2,1]

newtail = tail [10,9,8,7,6]

{- this is a comment
that goes over
several lines -}

double x = x + x

quadruple x = double (double x)

-- Factorial of a positive number
factorial n = product [1..n]

-- Average of a list of integers
average ns = sum ns `div` length ns

-- Question 3
n = a `div` length xs
     where
         a = 10
         xs = [1,2,3,4,5] 

add :: (Int,Int) -> Int
add (x,y) = x + y

add' :: Int -> Int -> Int
add' x y = x + y

even' n = n `mod` 2 == 0
splitAt' n xs = (take n xs, drop n xs)
recip' n = 1/n
abs' n = if n >= 0 then n else -n

abs'' n | n >= 0 = n
        | otherwise = -n

signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

not' False = True
not' True = False

test ('a':_) = True
test _       = False

const' x = \_ -> x 

sum' :: [Int] -> Int
sum' = foldl (+) 0

bigsum :: Integer
bigsum = sum [x^2 | x<-[1..100]]

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)
 
data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Nat = Zero | Succ Nat
   deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add'' :: Nat -> Nat -> Nat
add'' Zero      n = n
add'' (Succ m)  n = Succ (add'' m n)

data Colours = Red | Blue | Green | Yellow
     deriving Show
     
turnColour :: Colours -> Colours
turnColour Red    = Blue
turnColour Blue   = Green
turnColour Green  = Yellow
turnColour Yellow = Red
