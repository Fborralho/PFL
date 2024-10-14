import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Distribution.Simple.Setup (falseArg)
import Data.List (sortBy)
import GHC.Exts.Heap (GenClosure(value))

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

getFirstCity :: (City,City,Distance) -> City
getFirstCity (city1,_,_) = city1

getSecondCity :: (City,City,Distance) -> City
getSecondCity (_,city2,_) = city2

getDistance :: (City,City,Distance) -> Distance
getDistance (_,_,distance) = distance

removeRepeatedCities :: [City] -> [City]
removeRepeatedCities [] = []
removeRepeatedCities (x:xs)
    | x `elem` xs  = removeRepeatedCities xs
    | otherwise = x : removeRepeatedCities xs

cities :: RoadMap -> [City]
cities [] = []
cities (x:xs) = removeRepeatedCities ([getFirstCity x, getSecondCity x] ++ cities xs)

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any cityPair roadMap
    where
        cityPair(a,b,_) = ((a == city1) || (a == city2)) && ((b == city1) || (b == city2))

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2
    | areAdjacent roadMap city1 city2 = Just (getDistance (head (filter cityPair roadMap)))
    | otherwise = Nothing
  where
    cityPair (a, b, _) = (a == city1 && b == city2) || (a == city2 && b == city1)

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city = []
adjacent (x:xs) city
    | getFirstCity x == city = (getSecondCity x, getDistance x) : adjacent xs city
    | getSecondCity x == city = (getFirstCity x, getDistance x): adjacent xs city
    | otherwise = adjacent xs city

pathExists :: RoadMap -> Path -> Bool
pathExists [] _ = False
pathExists _ [] = True
pathExists _ [_] = True
pathExists roadMap (x:xs)
    | not (areAdjacent roadMap x (head xs)) = False
    | otherwise = pathExists roadMap xs

pathDistance' :: RoadMap -> Path -> Distance
pathDistance' [] _ = 0
pathDistance' _ [] = 0
pathDistance' _ [_] = 0
pathDistance' roadMap (x:y:xs) =
    case distance roadMap x y of
        Just d -> d + pathDistance' roadMap (y:xs)

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap path
    | not (pathExists roadMap path) = Nothing
    | otherwise = Just (pathDistance' roadMap path)

numberOfConnections :: RoadMap -> City -> Int
numberOfConnections [] _ = 0
numberOfConnections roadMap city = length (adjacent roadMap city)

getNumberOfConnectionsList :: RoadMap -> [City] -> [(City,Int)]
getNumberOfConnectionsList [] _ = []
getNumberOfConnectionsList _ [] = []
getNumberOfConnectionsList roadMap (x:xs) = (x,numberOfConnections roadMap x) : getNumberOfConnectionsList roadMap xs

filterConnections :: [(City,Int)] -> Int -> [City]
filterConnections [] _ = []
filterConnections (x:xs) value
    | snd x == value = fst x : filterConnections xs value
    | otherwise = filterConnections xs value


rome :: RoadMap -> [City]
rome [] = []
rome roadMap = filterConnections (getNumberOfConnectionsList roadMap (cities roadMap)) maxConnections
    where
        maxConnections = maximum (map snd (getNumberOfConnectionsList roadMap (cities roadMap)))

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True
isStronglyConnected roadMap
    |any cityPair (getNumberOfConnectionsList roadMap (cities roadMap)) = False
    |otherwise = True
        where 
            cityPair(_,connectionNumber) = connectionNumber /= length (cities roadMap) - 1


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- unconnected graph
gTest4 = [("Porto","Lisboa",450),("Porto","Aveiro",200),("Porto","Faro",500),("Lisboa","Setubal",90),("Lisboa","Faro",210)]

gTest5 :: RoadMap -- unconnected graph
gTest5 = [("0","1", 2), ("0","2", 1), ("1", "2", 0)]