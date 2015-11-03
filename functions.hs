perm :: Int -> Int -> Int
perm n r | r == 0    = 1
         | n == r    = if n == 0 then 1 else n * perm (n - 1) (r - 1)
         | otherwise = if r == 0 then 1 else n * perm (n - 1) (r - 1)

choose :: Float -> Float -> Float
choose n r | r == 0    = 1
           | n == r    = 1
           | otherwise = if r == 0 then 1 else (n / r) * choose (n - 1) (r - 1)

remainder :: Int -> Int -> Int
remainder a b = if a < b then a else remainder (a - b) b 
               
quotient :: Int -> Int -> Int
quotient a b | a < b     = 0
             | otherwise = quotient (a - b) b + 1


fib :: Float -> Float 
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib' 0 1 2   

       where 
         fib' :: Float -> Float -> Float -> Float
         fib' a b i = if i == n then a + b else fib' b (a + b) (i + 1)

gRatio :: Float -> Float
gRatio e = gRatio' e 1
-- e is the degree of precision 
            where 
             gRatio' :: Float -> Float -> Float
             gRatio' e k  | abs (x / y - z / x) < e = z / x
                          | otherwise               = gRatio' e (k + 1) 
                            where 
                               x = fib (k + 1)
                               y = fib k
                               z = fib (k + 2)
-- when abs () is greater or equal to e, gRatio' tries to find the next k until abs is smaller then e.


binary :: Int -> Int
binary d | d <  2    = d
         | otherwise = binary (d `div` 2) * 10 + d `mod` 2


newbase :: Int -> Int -> Int
newbase n b | b == 10   = n
            | otherwise = if n == 0 then 0 else (newbase (n `div` b) b) * 10 + n `mod` b

add2 :: Int -> Int -> Int
add2 x y | x == 0           = y
         | otherwise        = if y == 0 then x else add2 (succ x) (pred y) 

larger :: Int -> Int -> Int
larger x y = larger y x 
             where 
               larger' :: Int -> Int -> Int -> Int
               larger' a b = larger (a - 1) b (k + 1) 

primeFactors' :: Int -> Int -> [ Int ]
primeFactors' n i | n `mod` i /= 0                                                                  = []
                  | n `mod` i == 0 && (isPrime i == True) && (i < truncate (sqrt (fromIntegral n))) = i : primeFactors' (n `div` i) i

    
                                
