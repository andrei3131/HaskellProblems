module Lists where

import Data.List

myLength :: [a] -> Int
myLength []       = 0
myLength (x : xs) = 1 + myLength xs

elemIndex :: [a] -> Int -> a
elemIndex (x : xs) n
  | n < 0     = error "Negative Index"
  | otherwise = elemIndex' (x : xs) n
                 where
                    elemIndex' :: [a] -> Int -> a
                    elemIndex' [] n = error "Empty list"
                    elemIndex' (x : xs) n
                      | n == 0  = x  
                      | n > 0   = elemIndex' xs (n - 1)

myNull :: [a] -> Bool
myNull []       = True
myNull (x : xs) = False

append :: [a] -> [a] -> [a]
append [] (x : xs)       = x : xs
append (x : xs) []       = x : xs
append (x : xs) ys = x : append xs ys

myHead :: [a] -> a
myHead []       = error "Attempt to find head of empty list"
myHead (x : xs) = x

myTail :: [a] -> [a]
myTail []       = error "Attempt to finyd tail of empty list"
myTail (x : xs) = xs

myTake :: Int -> [a] -> [a]
myTake 0 (x : xs) = []
myTake n []       = error "Attempt to take elements from an empty list"
myTake n (x : xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 (x : xs) = x : xs
myDrop n []       = []
myDrop n (x : xs) = myDrop (n - 1) xs

myzip :: [a] -> [b] -> [(a, b)]		
myzip [] []             = []
myzip [] (x : xs)       = []
myzip (y : ys) []       = []
myzip (x : xs) (y : ys) = (x, y) : zip xs ys

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip []            = ([], [])
myUnzip ((x, y) : ps) = (x : xs, y : ys)
                         where
                          (xs ,ys) = myUnzip ps
mysum :: [Int] -> Int
mysum []       = 0
mysum (x : xs) = x + mysum xs   

myproduct :: [Int] -> Int
myproduct []       = 1
myproduct (x : xs) = x * myproduct xs

myand :: [Bool] -> Bool
myand []  =  True
myand (x : xs) 
  | x == True = myand xs
  | otherwise = False


myOr :: [Bool] -> Bool
myOr []  =  False
myOr (x : xs) 
  | x == False = myOr xs
  | otherwise  = True

myConcat :: [[a]] -> [a]
myConcat [[]]     = []
myConcat []       = []
myConcat (x : xs) = x ++ myConcat xs

myMaximum :: [Int] -> Int
myMaximum []       = error "Attempt to find maximum of an empty list"
myMaximum [a]      = a
myMaximum (x : xs) = max x (myMaximum xs)

myMinimum :: [Int] -> Int
myMinimum []       = error "Attempt to find minimum of an empty list"
myMinimum [a]      = a
myMinimum (x : xs) = min x (myMinimum xs)

mymap :: (a -> b) -> [a] -> [b]
mymap f []       = []
mymap f (x : xs) = f x : mymap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f []       = []
myFilter f (x : xs) 
  | f x       = x : myFilter f xs
  | otherwise = filter f xs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith (f) [] []             = []
myzipWith (f) [] (x : xs)       = []
myzipWith (f) (x : xs) []       = []
myzipWith (f) (x : xs) (y : ys) = (f) x y : myzipWith (f) xs ys 

precedes :: [Char] -> [Char] -> Bool
precedes [] []              = False
precedes [] (x : xs)        = True
precedes (x : xs) []        = False
precedes (x :  xs) (y : ys) 
  | x > y     = False
  | x == y    = precedes xs ys
  | otherwise = True

pos :: Int -> [Int] -> Int
pos n (x : xs)
 | n < 0     = error "Negative index"
 | otherwise = pos' n (x : xs)
                where
                 pos' :: Int -> [Int] -> Int
                 pos' 0 (x : xs) = 0
                 pos' n []       = error "Index too large"
                 pos' n (x : xs) 
                   | n == 1    = x
                   | otherwise = pos' (n - 1) xs

twoSame :: [Int] -> Bool
twoSame []       = False
twoSame (x : xs)  
  | elem x xs = True
  | otherwise = twoSame xs 
-- Complexity: O(n) Check if it is correct

isPrime :: Int -> Bool
isPrime n
  | n < 2          = False
  | n == 2         = True
  | n `mod` 2 == 0 = False
  | otherwise      = isPrime' n (truncate (sqrt (fromIntegral n)))
                       where 
                         isPrime' :: Int -> Int -> Bool
                         isPrime' x i
                           | i == 1         = True
                           | x `mod` i == 0 = False
                           | otherwise      = (isPrime' x (i - 1) && x `mod` i /= 0)
              
primeFactors :: Int -> [Int]
primeFactors n 
  = primeFactors' n 2
     where
      primeFactors' :: Int -> Int -> [Int]
      primeFactors' x i
        | isPrime x      = [x]
        | i > x `div` 2  = [] 
        | x `mod` i == 0 = i : primeFactors' (x `div` i) i
        | x `mod` i /= 0 = primeFactors' x (i + (i `mod` 2) + 1)
         
hcf :: Int -> Int -> Int
hcf a b
 | a == 0 && b == 0 = error "Both numbers are 0" 
 | a == 0           = b
 | b == 0           = a  
 | otherwise        = product (primeFactors a \\ (primeFactors a \\ primeFactors b))

mylcm :: Int -> Int -> Int
mylcm a b
  | a < b  = a * product (primeFactors b \\ primeFactors a)
  | a == b = a
  | a > b  = mylcm b a 

right :: a -> [a] -> [a]
right n []       = [n]
right n (x : xs) = x : right n xs

backwards :: Eq a => [a] -> [a]
backwards []       = []
backwards (x : xs) = (backwards xs) ++ (right x xs \\ xs)
-- O(n^2)

myBackwards :: [a] -> [a]
myBackwards (x : xs) 
  = myBackwards' (x : xs) []
     where
      myBackwards' :: [a] -> [a] -> [a]
      myBackwards' [] l        = l    
      myBackwards' (x : xs) l = myBackwards' xs (x : l)                 

--transpose :: [Char] -> [Char] -> [Char] -> [Char]
--transpose [] l' l''  = []
--transpose l l' l'' 
-- | length l' /= length l''                         = error "Anagrams do not have the same length"
-- length l /= length l' || length l /= length l'' = error "Lists are not of the same length"
-- | otherwise                                       = transpose' l l' l'' []
--                                                       where
  --                                                      transpose' :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
    --                                                    transpose' [] [] [] acc = acc 
      --                                                  transpose' (x : xs) (y : ys) (z : zs) acc
        --                                                  | elem y (z : zs) && pos y (z : zs) == length (z : zs) = transpose' xs ys (z : zs) (x : acc)
          --                                                | elem y (z : zs) && pos y (z : zs) < length (z : zs)  = transpose' xs ys zs acc


suf :: String -> [String]
suf []       = []
suf (x : xs) = (x : xs) : suf xs

substring :: String -> String -> Bool
substring l l' 
  = or (substring' l l')
     where 
      substring' :: String -> String -> [Bool]
      substring' [] []      = [True]
      substring' l (y : ys) = [compare l suffix | suffix <- suf (y : ys)]
       where
        compare :: String -> String -> Bool
        compare [] []             = True
        compare [] (x : xs)       = True
        compare (x : xs) []       = False
        compare (x : xs) (y : ys) = (x == y) && compare xs ys

merge2 :: [Int] -> [Int] -> [Int]
merge2 [] (y : ys)       = error "Empty list"
merge2 (x : xs) []       = error "Empty list"
merge2 (x : xs) (y : ys) = qSort l
                            where
                             l = (x : xs) ++ (y : ys)
qSort :: [Int] -> [Int]
qSort [] = []
qSort (z : zs) = qSort [a | a <- zs, a <= z] ++ [z] ++ qSort [a | a <- zs, a > z] 

whitespace = [' ', '\n', '\t']

getWord :: String -> String
getWord [] = []
getWord (x : xs)
  | elem x whitespace = []
  | otherwise         = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x : xs)
  | elem x whitespace = tail (x : xs)
  | otherwise         = dropWord xs
                       
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x : xs)
  | elem x whitespace = dropSpace xs
  | otherwise         = (x : xs)

nextWord :: String -> (String, String)
nextWord [] = ([], [])
nextWord (x : xs) = (getWord (x : xs), dropWord (x : xs))




