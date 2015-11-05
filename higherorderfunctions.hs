import Data.List

merge2 :: [Int] -> [Int] -> [Int]
merge2 xs [] = xs
merge2 [] xs = xs
merge2 (x : xs) (y : ys)
  | x == y    = x : y : merge2 xs ys
  | x < y     = x : merge2 xs (y : ys)
  | otherwise = y : merge2 (x : xs) ys 

mergeN :: [[Int]] -> [Int]
mergeN (l : ls)
  = foldr (merge2) [] (l : ls)

mergeN' :: [[Int]] -> [Int]
-- Pre: All elements are non-null
mergeN' []
  = []
mergeN' xs
  = m : mergeN' (remove m xs)
  where
  m = minimum (map minimum xs)

remove :: Int -> [[Int]] -> [[Int]]
remove i []
  = []
remove i ((x : xs) : ys)
  | i == x    = if null xs then ys else xs : ys
  | otherwise = (x : xs) : remove i ys 

same :: [Int] -> Bool
same xs
  = and (zipWith (==) xs (tail xs))

myscanl1 :: (a -> a -> a) -> [a] -> [a]
myscanl1 f [] 
  = []
myscanl1 f (x : xs)
  = scanl f x xs

myscanr1 :: (a -> a -> a) -> [a] -> [a]
myscanr1 f []
  = []
myscanr1 f [x]
  = [x]
myscanr1 f (x : xs)
  = (f x q) : ys
  where
  ys@(q : _) = myscanr1 f xs

-- x to the power n

powern :: Int -> Int -> Int
powern x 0
  = 1
powern x n
  | even n = k * k
  | odd n  = x * k * k 
           where
           k = powern x (n `div` 2)

mysqrt :: Float -> Float
mysqrt x 
  = sqrt' (x / 2)
  where
  sqrt' :: Float -> Float
  sqrt' a
    | abs (x - a * a) / x < 0.0000001 = a
    | otherwise                       = sqrt' ((a + x / a) / 2)

fib :: Int -> [Int]
fib n
   = take n (inffib) 

inffib 
  = 1 : scanl (+) 1 inffib 

insert' :: Int -> [Int] -> [Int]
-- Pre: list is ordered
insert' n []
  = []
insert' n (x : xs)
  | n > x     = x : insert' n xs
  | otherwise = n : (x : xs)


iSort :: [Int] -> [Int]
iSort []
  = []
iSort (x : xs)
  = insert x (iSort xs)

factorials
  = scanl (*) 1 [1..] 
--infinite list of factorials

aproxe
  = sum (take 10 (map (1/) factorials))


fibinfinite
  = let x = 1 : scanl (+) 1 x in x

qSort :: [Int] -> [Int]
qSort []
  = []
qSort (x : xs)
  = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]

squash :: (a -> a -> b) -> [a] -> [b]
squash f xs
  = map (uncurry f) (zip xs (tail xs))

squash' :: (a -> a -> b) -> [a] -> [b]
squash' f xs
   = zipWith f xs (tail xs)

convergence :: (a -> a -> Bool) -> [a] -> a
convergence f [x]
  = x
convergence f (x : y : ys)
   | f x y     = x
   | otherwise = convergence f (y : ys)

compe :: Float
compe 
 = convergence lim (scanl (+) 0 (map (1/) (scanl (*) 1 [1..])))
 where
 lim x y = abs (x - y) < 0.00001

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f [] 
  = []
limit f [x]
  = [x]
limit f (x : y : ys)
  | not (f x y) = x : limit f (y : ys)
  | otherwise   = x : y : ys

myany, myall :: (a -> Bool) -> [a] -> Bool
myany p 
  = or . (map p)
myall p 
  = and . (map p)

elem' :: Eq a => a -> [a] -> Bool
elem'
  = any . (==)

infixl 9 <.>
(<.>) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
f <.> g
   = h
   where
   h x y = f (g x y) 

myany', myall' :: (a -> Bool) -> [a] -> Bool
myany'
  = or <.> map
myall'
  = and <.> map

pipeline :: [a -> a] -> [a] -> [a]
pipeline
  = map . (foldr (.) id)




