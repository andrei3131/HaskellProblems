{-data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Eq, Ord, Show)




treesize :: Tree a -> Int
treesize (Leaf n)
  = 0
treesize (Node l r)
 = 1 + treesize l + treesize r

build :: [Int] -> Tree Int
build []
  = error "empty list"
build [x]
  = Leaf x
build xs
  = Node (build cs) (build ds)
  where
  (cs, ds) = splitAt ((length xs) `div` 2) xs

ends :: Tree Int -> [Int]
ends (Leaf x)
 = [x]
ends (Node l r)
 = ends l ++ ends r

swap :: Tree Int -> Tree Int
swap (Leaf x)
 = Leaf x
swap (Node l r)
 = Node (swap r) (swap l)


data Queue = Empty | Person String 
           deriving (Eq, Ord, Show)

nobody :: Queue -> [Queue]
nobody Empty 
  = [Empty]

arrive :: Queue -> [Queue] -> [Queue]
arrive (Person ns) [Empty]
  = [Person ns]
arrive (Person ns) qs
  = reverse ((Person ns) : reverse qs)

first :: [Queue] -> Queue
first (q : qs)
  = q


serve :: [Queue] -> [Queue]
serve (q : qs)
  = qs

data AmPm = AM | PM 
data Time = T24 Int | T12 Int Int AmPm
   


to24 :: Time -> Time
to24 (T12 h m AM)
  = T24 (h * 100 + m)
to24 (T12 h m PM)
  = T24 ((h + 12) * 100 + m)

equaltime :: Time -> Time -> Bool
equaltime (T24 a) (T24 b)
  = a == b
equaltime (T24 a) (T12 h m AM)
  = equaltime (T24 a) (to24 ((T12 h m AM)))
equaltime (T12 h m AM) (T24 a)
  = equaltime (to24 ((T12 h m AM))) (T24 a)  
equaltime (T24 a) (T12 h m PM)
  = equaltime (T24 a) (to24 ((T12 h m PM)))
equaltime (T12 h m PM) (T24 a)
  = equaltime (to24 ((T12 h m PM))) (T24 a)  

instance Eq AmPm where
  AM == AM = True
  PM == PM = True

instance Eq Time where
  T24 a == T24 a'            = a == a'
  T12 h m AM == T12 h' m' AM = h == h' && m == m'
  T12 h m PM == T12 h' m' PM = h == h' && m == m'
  T12 00 m AM == T24 t       = (t `div` 100) == 24 && m == (t `mod` 100)
  T12 h m AM == T24 t        = h == (t `div` 100) && (m == (t `mod` 100))
  T12 h m PM == T24 t        = h == ((t `div` 100) `mod` 12) && (m == t `mod` 100)

instance Show Time where
  show (T24 t)        = show t ++ "HRS"
  show (T12 12 00 AM) = "Midnight"
  show (T12 12 00 PM) = "Midday"
  show (T12 h m AM)   = show h ++ ":" ++ show m ++ "AM"
  show (T12 h m PM)   = show h ++ ":" ++ show m ++ "PM"

-}


data Tree3 a b = Empty | Leaf a | Node (Tree3 a b) b (Tree3 a b)
             deriving (Eq, Ord, Show)

map3 ::(b -> d) -> (a -> c) -> Tree3 a b -> Tree3 c d
map3 fn fl Empty
  = Empty
map3 fn fl (Leaf a)
  = Leaf (fl a)
map3 fn fl (Node l x r)
  = Node (map3 fn fl l) (fn x) (map3 fn fl r)
            -- Leaf                -- Node
{-foldT3 :: (a -> c -> c) -> c -> (b -> d -> d) -> d -> Tree3 a b -> (c, d)
foldT3 f el g en Empty
  = (el , en)
foldT3 f el g en (Leaf x)
  = (f x el, en)
foldT3 f el g en (Node l x r)
  -} 



