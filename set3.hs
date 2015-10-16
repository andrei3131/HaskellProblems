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
