import Data.List

--
-- Predefined types and a sample list of requests
--

type Name = String

type Address = String

type Time = Int

type TimeHM = (Int, Int)

type Request = (Name, Address, Time, Time)

rs :: [Request]
rs = [("Killer","11 Manton Av",1000,1130),
      ("Rex","4 Howard Court",1500,1615),
      ("Jaws","1 West Rd",1030,1115),
      ("Gnasher","16 Park St",1200,1330),
      ("Satan","Hamley Manor",900,1015),
      ("Fangs","19 Clover St",1400,1530),
      ("Preston","44 Main St",1145,1345),
      ("Chomp","9 Radley St",1545,1730)]

---------------------------------------------------------
startTime, finishTime :: Request -> Time
startTime (name, address, sTime, fTime)
  = sTime

finishTime (name, address, sTime, fTime)
  = fTime

convertTime :: Int -> TimeHM
convertTime t
  = (t `div` 100, t `mod` 100)


subtractTimes :: Int -> Int -> Int
--Pre: t1 >= t2
subtractTimes t1 t2
  = ((fst a) - (fst b)) * 60  + ((snd a) - (snd b))
  where
  a = convertTime t1  
  b = convertTime t2  


sortRequests :: [Request] -> [Request]
sortRequests (req@(name, address, sTime, fTime) : rest)
 = sortRequests' (qSort [finishTime ft | ft <- (req : rest)]) (req : rest)
 where
 sortRequests' :: [Int] -> [Request] -> [Request]
 sortRequests' (x : xs) (r : rs)
   = [ req | ftime <- (x : xs), req <- (r : rs), ftime == finishTime req ]  
   
             
qSort :: [Int] -> [Int]
qSort []
 = []
qSort (x : xs) 
 = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]

schedule :: [Request] -> [Request]
schedule (r : rs)
  = schedule' (sortRequests (r : rs))
  where
  schedule' :: [Request] -> [Request]
  schedule' [] = []
  schedule' (r : rs) = r : schedule' [ y | y <- rs, startTime y > finishTime r]        

breaks :: [Request] -> [Int]
--Pre: List of requests forms a valid schedule
breaks [x] = []
breaks (r : rs)
  = (subtractTimes (startTime (head rs)) (finishTime r)) : breaks rs
