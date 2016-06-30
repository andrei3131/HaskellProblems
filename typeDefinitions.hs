import Data.List

pos :: Eq a => a -> [a] -> Int
pos n (x : xs)
  | n == x    = 0 
  | otherwise = 1 + pos n xs
 

transpose' :: String -> String -> String -> String
transpose' an s [] 
  = []
transpose' an s (r : rs)
  = an !! (pos r s) : transpose' an s rs

nextWord :: String -> (String, String)
nextWord []
  = ([], [])
nextWord (x : xs)
  | elem x " \t\n" = ([], xs)
  | otherwise      = (x : ws, cs)
                   where
                   (ws, cs) = nextWord xs 

splitUp :: String -> [String]
splitUp []
  = []
splitUp (x : xs)
  | elem x " \t\n" = splitUp xs 
  | otherwise      = word : splitUp ((x : xs) \\ word )
                   where
                   word = fst (nextWord (x : xs))
findAll x db = [y | (x', y) <- db, x == x']

timesTable :: Int -> Int -> [String]
timesTable a b = [ (show x) ++ " times " ++ (show y) ++ " is " ++ show (x * y) | x <- [1..a], y <- [1..b] ]

perm :: Eq a => [a] -> [[a]]
perm []
  = [[]]
perm xs
  = [ x : ps | x <- xs, ps <- perm (xs \\ [x]) ]


subpops :: [Int] -> Int -> [[Int]]
subpops (p : ps) k 
  = [  s : ss     | s <- [0..p], ss <- subpops ps (k - s)]
subpops ps k
  = if k == 0 then [[]] else []


data Shape = Triangle Float Float Float | Square Float | Circle Float
              deriving (Eq, Ord, Show) 


area :: Shape -> Float
area (Triangle a b c)
   = sqrt (s * (s - a) * (s - b) * (s - c))
   where
   s = (a + b + c) / 2
area (Square l)
   = l * l
area (Circle r)
   = pi * r ^ 2

data ShapeCart = Polygon [(Float, Float)]


area' :: ShapeCart -> Float
area' (Polygon [a, b])
  = 0
area' (Polygon (v : v' : v'' : vs))
  = area (Triangle (len v v') (len v' v'') (len v'' v)) + area' (Polygon (v : v'' : vs)) 
  where
  len (x, y) (x', y') = sqrt ((x' - x) ^ 2 + (y' - y) ^ 2)

removeSecond :: [(Float, Float)] -> [(Float, Float)]
removeSecond []
  = []
removeSecond [x]
  = []
removeSecond (v : v' : vs)    
  = v : vs

data Date = DDMMYYYY Int Int Int
            deriving (Eq, Ord, Show)

age :: Date -> Date -> Int
age (DDMMYYYY d m y) (DDMMYYYY d' m' y')
  | m < m'    = y' - y 
  | m == m'   = if d' < d then y' - y - 1 else y' - y
  | otherwise = y' - y - 1




data Possibly a = Failure | Success a
                deriving (Eq, Ord, Show)

tableLookUp :: Eq a => a -> [(a, b)] -> Possibly b
tableLookUp x []
  = Failure
tableLookUp x (p : ps)
  | fst p == x = Success (snd p)
  | otherwise  = tableLookUp x ps 


database1 
 = [TeachingStaff Software (Coursenumber 150) (Name "Ion Popescu") (Male) (DDMMYY 10 08 1950) (Salary 10000), TeachingStaff Theory (Coursenumber 120) (Name "Andrei B") (Male) (DDMMYY 31 07 1996) (Salary 50000), SupportStaff (Name "Georgeta C") (Female) (DDMMYY 15 05 1990) (Salary 5000) ]


data Course   = Coursenumber Int
              deriving (Eq, Ord, Show) 
data Empdata  = Name String | Male | Female | DDMMYY Int Int Int | Salary Float
              deriving (Eq, Ord, Show)
data Research = Systems | Software | Theory
              deriving (Eq, Ord, Show)
data Employee = TeachingStaff Research Course Empdata Empdata Empdata Empdata| SupportStaff Empdata Empdata Empdata Empdata
              deriving (Eq, Ord, Show)

type Ustaff = Employee

type Database = [Ustaff]

name :: Ustaff -> Empdata
name (TeachingStaff _ _ name _ _ _)
  = name
name (SupportStaff name _ _ _)
  = name

salary :: Ustaff -> Empdata
salary (TeachingStaff _ _ _ _ _ s)
  = s
salary (SupportStaff _ _ _ s)
  = s

isSupport :: Ustaff -> Bool
isSupport (SupportStaff _ _ _ _)
  = True
isSupport (TeachingStaff _ _ _ _ _ _)
  = False

totalsupport :: Database -> Int
totalsupport []
  = 0
totalsupport (db : dbs)
  | isSupport db = 1 + totalsupport dbs
  | otherwise    = totalsupport dbs

teaches :: Course -> Ustaff -> Bool
teaches (Coursenumber n) (SupportStaff _ _ _ _)
  = False
teaches (Coursenumber n) (TeachingStaff _ (Coursenumber x) _ _ _ _)
  | n == x    = True
  | otherwise = False

taughtby :: Course -> Database -> Empdata
taughtby (Coursenumber n) (db : dbs)
   | teaches (Coursenumber n) db = name db
   | otherwise                   = taughtby (Coursenumber n) dbs

salarybill :: Database -> Empdata
salarybill []
  = Salary 0
salarybill (db : dbs)
  = addSalary (salary db) (salarybill dbs)


addSalary :: Empdata -> Empdata -> Empdata
addSalary (Salary a) (Salary b)
  = Salary (a + b)


t1, t2, t3, t4, t5, t6 :: Tree Int

t1    = Node (Node Empty 1 (Node Empty 4 Empty)) 7
          (Node (Node Empty 6 Empty) 12 Empty)
t2    = Node Empty 6 Empty
t3    = Node (Node Empty 4 Empty) 9 Empty
t4    = Node (Node Empty 4 Empty) 9 (Node Empty 17 Empty)
t5    = Node (Node t3 7 t4) 3 t1
t6    = Node (Node (Node Empty 3 (Node Empty 7 Empty)) 9 (Node (Node Empty 10 Empty) 12 Empty)) 15 (Node Empty 22 (Node Empty 50 Empty)) 


data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Eq, Ord, Show)

--insert in an ordered tree

insertT :: Int -> Tree Int -> Tree Int
insertT n Empty
  = Node Empty n Empty
insertT n (Node l x r)
  | n <= x    = Node (insertT n l) x r
  | otherwise = Node l x (insertT n r)

flatten :: Tree Int -> [Int]
flatten Empty
  = []
flatten (Node l x r)
  = flatten l ++ (x : flatten r)

build :: [Int] -> Tree Int
build
  = foldr insertT Empty

treesort :: [Int] -> [Int]
treesort 
  = flatten . build

data D = C Int

instance Eq D

data Tree' = Leaf' | Node' Tree' Tree'
             deriving (Eq, Ord, Show)

makeTrees :: Int -> [Tree']
makeTrees 0
  = [Leaf']
makeTrees 1
  = [Node' Leaf' Leaf']
{-makeTrees 2
  = [ Node Leaf a | a <- [Node Leaf Leaf] ] ++ [ Node a Leaf | a <- [Node Leaf Leaf] ]
makeTrees 3
  = [ Node Leaf a | a <- ([Node Leaf Leaf] ++ makeTrees 2)] ++ [ Node a Tree | a <- [Node Leaf Leaf] ++ makeTrees 2 ] -}
makeTrees n
  = [Node' Leaf' a | a <- makeTrees (n - 1)] ++ [Node' a Leaf' | a <- makeTrees (n - 1)] 







